/**
 * Copyright 2011-2013 eBusiness Information, Groupe Excilys (www.ebusinessinformation.fr)
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 * 		http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package com.excilys.ebi.gatling.http.cookie

import java.net.URI

import scala.annotation.tailrec

import com.ning.http.client.Cookie

object CookieJar {

	// rfc6265#section-5.1.3.
	def domainMatches(domain: String, host: String): Boolean = {
		if (domain == null || host == domain) {
			true
		} else if (host.length > domain.length) {
			host.endsWith(domain) &&
				host.charAt(host.length - domain.length - 1) == '.'
			//TODO check that the host is not an IP address
		} else {
			false
		}
	}

	def apply(uri: URI, cookies: List[Cookie]) = (new CookieJar(Map.empty)).add(uri, cookies)
}

/*
 * rfc2109 and rfc2965 are now deprecated by rfc6265 (http://tools.ietf.org/html/rfc6265) 
 */
private[cookie] class CookieJar(store: Map[URI, List[Cookie]]) {

	import CookieJar.domainMatches

	private val MAX_AGE_UNSPECIFIED = -1L
  private val DOMAIN_UNSPECIFIED = ""

	private def getEffectiveUri(uri: URI) =
		new URI(null, // scheme
			uri.getHost, // rfc6265#section-1 Cookies for a given host are shared  across all the ports on that host
			null, // path component
			null, // query component
			null) // fragment component

	private def extractDomain(rawURI: URI, cookie: Cookie) = Option(cookie.getDomain)
		// rfc6265#section-5.2.3  Let cookie-domain be the attribute-value without the leading %x2E (".") character.
		.map(dom => if (dom.startsWith(".")) dom.substring(1).toLowerCase else dom.toLowerCase)
		// rfc6265#section-1 Cookies for a given host are shared  across all the ports on that host
		.getOrElse(DOMAIN_UNSPECIFIED)

	/**
	 * @param uri       the uri this cookie associated with.
	 *                  if <tt>null</tt>, this cookie will not be associated
	 *                  with an URI
	 * @param cookie    the cookie to store
	 */
	def add(rawURI: URI, rawCookies: List[Cookie]): CookieJar = {
		val newCookies = rawCookies.map { cookie =>
			val fixedDomain = extractDomain(rawURI, cookie)
			val fixedPath = Option(cookie.getPath).getOrElse(rawURI.getPath)

			if (fixedDomain != cookie.getDomain || fixedPath != cookie.getPath)
				new Cookie(fixedDomain, cookie.getName, cookie.getValue, fixedPath, cookie.getMaxAge, cookie.isSecure, cookie.getVersion, cookie.isHttpOnly, cookie.isDiscard, cookie.getComment, cookie.getCommentUrl, cookie.getPorts)
			else
				cookie
		} filter {
			// Reject the cookies when the domains don't match, cf: RFC 2965 sec. 3.3.2
			cookie => domainMatches(cookie.getDomain, rawURI.getHost)
		}

		def cookiesEquals(c1: Cookie, c2: Cookie) = {

      if(c1 == null && c2 ==null ){
        true
      } else if (c1 != null && c1.compareTo(c2) == 0){
        true
      } else {
        false
      }
    }

		@tailrec
		def addOrReplaceCookies(newCookies: List[Cookie], oldCookies: List[Cookie]): List[Cookie] = newCookies match {
			case Nil => oldCookies
			case newCookie :: moreNewCookies =>
				val updatedCookies = newCookie :: oldCookies.filterNot(cookiesEquals(_, newCookie))
				addOrReplaceCookies(moreNewCookies, updatedCookies)
		}

		def hasExpired(c: Cookie): Boolean = c.getMaxAge != MAX_AGE_UNSPECIFIED && c.getMaxAge <= 0

		val uri = getEffectiveUri(rawURI)
		val cookiesWithExactURI = addOrReplaceCookies(newCookies, store.get(uri).getOrElse(List.empty))
		val nonExpiredCookies = cookiesWithExactURI.filterNot(hasExpired)
		new CookieJar(store + (uri -> nonExpiredCookies))
	}

	def get(rawURI: URI): List[Cookie] = {

		val fixedPath = if (rawURI.getPath.isEmpty) "/" else rawURI.getPath
		val uri = getEffectiveUri(rawURI)

		def pathMatches(cookie: Cookie) = fixedPath.startsWith(Option(cookie.getPath).getOrElse("/"))

		val cookiesWithExactDomain = store.get(uri).getOrElse(Nil).filter(pathMatches)
		val cookiesWithExactDomainNames = cookiesWithExactDomain.map(_.getName.toLowerCase)

		// known limitation: might return duplicates if more than 1 cookie with a given name with non exact domain
		val cookiesWithMatchingDomain = store
			.filterKeys(_ != uri)
			.values
			.flatten
			.filter(cookie => !cookiesWithExactDomainNames.contains(cookie.getName.toLowerCase)
				&& domainMatches(cookie.getDomain, rawURI.getHost) && pathMatches(cookie))

		// known limitation: don't handle runtime expiration, intended for stress test
		cookiesWithExactDomain ++ cookiesWithMatchingDomain
	}

	override def toString = "CookieStore=" + store.toString
}