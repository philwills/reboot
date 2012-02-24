package dispatch.spec

import org.scalacheck._

object AlphaParamsSpecification
  extends Properties("Basic")
  with unfiltered.spec.ServerCleanup {

  import Prop.forAll

  val server = {
    import unfiltered.netty
    import unfiltered.response._
    import unfiltered.request._
    object Echo extends Params.Extract("echo", Params.first)
    netty.Http.anylocal.handler(netty.cycle.Planify {
      case req@Path("/echo") & Params(Echo(echo)) =>
        PlainTextContent ~> {
          ResponseHeader(req.method, List(echo)) ~>
          ResponseString(req.method + echo)
        }
    }).start()
  }

  import dispatch._

  def localhost = host("127.0.0.1", server.port)

  property("POST and handle") = forAll(Gen.alphaStr) {
    (sample: String) =>
      val res = Http(
        (localhost / "echo" POST ("echo" -> sample)) > As.string
      )
      res() == ("POST" + sample)
  }

  property("GET and handle") = forAll(Gen.alphaStr) {
    (sample: String) =>
      val res = Http(
        (localhost / "echo" GET ("echo" -> sample)) > As.string
      )
      res() == ("GET" + sample)
  }

  property("PUT and handle") = forAll(Gen.alphaStr) {
    (sample: String) =>
      val res = Http(
        (localhost / "echo" PUT ("echo" -> sample)) > As.string
      )
      res() == ("PUT" + sample)
  }

  property("HEAD and handle") = forAll(Gen.alphaStr) {
    (sample: String) =>
      val res = Http(
        (localhost / "echo" HEAD ("echo" -> sample)) > As (_.getHeader("HEAD"))
      )
      res() == (sample)
  }

  property("DELETE and handle") = forAll(Gen.alphaStr) {
    (sample: String) =>
      val res = Http(
        (localhost / "echo" DELETE ("echo" -> sample)) > As.string
      )
      res() == ("DELETE" + sample)
  }

  property("PATCH and handle") = forAll(Gen.alphaStr) {
    (sample: String) =>
      val res = Http(
        (localhost / "echo" PATCH ("echo" -> sample)) > As.string
      )
      res() == ("PATCH" + sample)
  }

  // TODO: Work out sensible testing approach for GET, POST, et with no parameters
}
