package dispatch

import com.ning.http.client.RequestBuilder

class DefaultRequestVerbs(val subject: RequestBuilder)
extends MethodVerbs with UrlVerbs with ParamVerbs with AuthVerbs

trait HostVerbs {
  def apply(host: String) =
    new RequestBuilder().setUrl("http://%s/".format(host))
  def apply(host: String, port: Int) =
    new RequestBuilder().setUrl("http://%s:%d/".format(host, port))
}
object :/ extends HostVerbs
object host extends HostVerbs

object url extends (String => RequestBuilder) {
  def apply(url: String) = new RequestBuilder().setUrl(url)
}
trait RequestVerbs {
  def subject: RequestBuilder
}

trait MethodVerbs extends RequestVerbs {
  def HEAD   = subject.setMethod("HEAD")
  def GET    = subject.setMethod("GET")
  def POST   = subject.setMethod("POST")
  def PUT    = subject.setMethod("PUT")
  def DELETE = subject.setMethod("DELETE")
  def PATCH  = subject.setMethod("PATCH")
}

trait UrlVerbs extends RequestVerbs {
  import java.net.URI
  def url = subject.build.getUrl // unfortunate
  def / (path: String) = subject.setUrl(url match {
    case u if u.endsWith("/") => u + path
    case u => u + "/" + path
  })
  def secure = {
    val uri = URI.create(url)
    subject.setUrl(new URI(
      "https", uri.getAuthority, uri.getPath, uri.getQuery, uri.getFragment
    ).toString)
  }
}

trait ParamVerbs extends RequestVerbs {
  def << (params: Traversable[(String,String)]) =
    (subject.setMethod("POST") /: params) {
      case (s, (key, value)) =>
        s.addParameter(key, value)
    }
  def <<? (params: Traversable[(String,String)]) = withQueryParams(params, "GET")
  def withQueryParams(params: Traversable[(String, String)], method: String) =
    (subject.setMethod(method) /: params) {
      case (s, (key, value)) =>
        s.addQueryParameter(key, value)
    }

  def HEAD (params: (String, String)*) = withQueryParams(params.toTraversable, "HEAD")
  def GET (params: (String, String)*) = <<?(params.toTraversable)
  def POST (params: (String, String)*) = <<(params.toTraversable)
  def PUT (params: (String, String)*) = withQueryParams(params.toTraversable, "PUT")
  def DELETE (params: (String, String)*) = withQueryParams(params.toTraversable, "DELETE")
  def PATCH (params: (String, String)*) = withQueryParams(params.toTraversable, "PATCH")
}

trait AuthVerbs extends RequestVerbs {
  import com.ning.http.client.Realm.RealmBuilder
  def as (user: String, password: String) =
    subject.setRealm(new RealmBuilder()
                     .setPrincipal(user)
                     .setPassword(password)
                     .build())
}
