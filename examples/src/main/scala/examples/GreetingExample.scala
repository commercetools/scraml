package examples

import akka.actor.ActorSystem
import akka.http.scaladsl.Http
import cats.effect.{ExitCode, IO, IOApp}
import scraml.examples.{DataType, Endpoints}
import scraml.examples.Endpoints.Greeting.GetGreetingParams
import sttp.client3.SttpBackend
import sttp.client3.asynchttpclient.cats.AsyncHttpClientCatsBackend
import sttp.model.{Header, StatusCode}
import sttp.tapir.DecodeResult.{Failure, Value}
import sttp.tapir.client.sttp.WebSocketToPipe

import scala.concurrent.Future

class GreetingClient(apiUrl: String)(backend: SttpBackend[IO, Any])(implicit wsToPipe: WebSocketToPipe[Any]) {
  import sttp.client3._
  import sttp.tapir._
  import sttp.tapir.client.sttp.SttpClientInterpreter

  private lazy val client = SttpClientInterpreter()
  private def authenticate: IO[String] = IO.pure("sometoken")

  def getGreeting(params: GetGreetingParams): IO[DataType] = for {
    accessToken <- authenticate
    // adding an input and output to the endpoint to access headers and to provide an access token (checking not implemented)
    response <- client.toClient(Endpoints.Greeting.getGreeting.in(auth.bearer[String]()).out(headers), Some(uri"$apiUrl"), backend)(wsToPipe)(params, accessToken)
    result <- response match {
      case Value(value) => IO.fromEither(value.left.map(error => new RuntimeException(s"error in $response: $error")))
      case error: Failure => IO.raiseError(new RuntimeException(s"error while getting greeting: $error"))
    }
    (data, headers) = result
    _ <- IO(println(s"got headers: $headers"))
  } yield data
}

object GreetingClient {
  def apply(apiUrl: String): IO[(GreetingClient, SttpBackend[IO, Any])] =
    AsyncHttpClientCatsBackend[IO]().flatMap(backend => IO((new GreetingClient(apiUrl)(backend), backend)))
}

object GreetingServer {
  import sttp.tapir._
  import sttp.tapir.server.akkahttp.AkkaHttpServerInterpreter
  import scala.concurrent.Future
  import akka.http.scaladsl.server.Route

  def getGreeting(params: GetGreetingParams): Future[Either[Unit, (DataType, StatusCode, List[Header])]] =
    Future.successful(Right(DataType(params.name.getOrElse("no input"), customTypeProp = BigDecimal(42)), StatusCode.Ok, List(Header("custom-header", "value"))))

  // adding outputs to provide statusCode and headers in the implementation
  val greetingWithStatusAndHeaders = Endpoints.Greeting.getGreeting.out(statusCode and sttp.tapir.headers)

  val greetingRoute: Route =
    AkkaHttpServerInterpreter().toRoute(greetingWithStatusAndHeaders)(getGreeting)

  implicit val httpSystem: ActorSystem = ActorSystem("http")

  def startServer: IO[Http.ServerBinding] =
    IO.fromFuture(IO(Http().newServerAt("localhost", 8080).bind(greetingRoute)))
}

object GreetingApp extends IOApp {
  implicit class FutureOps[T](future: Future[T]) {
    def toIO: IO[T] = IO.fromFuture(IO(future))
  }

  override def run(args: List[String]): IO[ExitCode] = for {
    binding <- GreetingServer.startServer
    (clientWithBackend) <- GreetingClient(
      apiUrl = s"http://${binding.localAddress.getHostName}:${binding.localAddress.getPort}"
    )
    (client, clientBackend) = clientWithBackend
    result <- client.getGreeting(GetGreetingParams(name = Some("world"))).attempt
    _ <- IO(println(result))
    _ <-
      clientBackend
        .close()
        .guarantee(binding.unbind().toIO.void)
        .guarantee(GreetingServer.httpSystem.terminate().toIO.void)
  } yield ExitCode.Success
}
