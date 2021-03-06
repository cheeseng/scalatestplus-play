/*
 * Copyright 2001-2014 Artima, Inc.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package org.scalatestplus.play

import play.api.test._
import org.scalatest._
import selenium.WebBrowser
import concurrent.Eventually
import concurrent.IntegrationPatience
import org.openqa.selenium.WebDriver
import org.openqa.selenium.firefox.FirefoxDriver
import org.openqa.selenium.firefox.FirefoxProfile
import play.api.http.{ HttpProtocol, Status, HeaderNames }

/**
 * Convenience "super Suite" base class for Play tests.
 *
 * Extend this class by default for testing Play apps with the ScalaTest + Play library. You can mix other traits into it to access needed fixtures, such as
 * [[org.scalatestplus.play.OneAppPerSuite OneAppPerSuite]], [[org.scalatestplus.play.OneAppPerTest OneAppPerTest]], [[org.scalatestplus.play.OneServerPerSuite OneServerPerSuite]], [[org.scalatestplus.play.OneServerPerTest OneServerPerTest]], [[org.scalatestplus.play.OneBrowserPerSuite OneBrowserPerSuite]], [[org.scalatestplus.play.OneBrowserPerTest OneBrowserPerTest]], [[org.scalatestplus.play.AllBrowsersPerSuite AllBrowsersPerSuite]], or [[org.scalatestplus.play.AllBrowsersPerTest AllBrowsersPerTest]] mix If you want to use trait [[org.scalatestplus.play.MixedFixtures MixedFixtures]], extend [[org.scalatestplus.play.MixedPlaySpec MixedPlaySpec]] instead.
 */
abstract class PlaySpec extends WordSpec with MustMatchers with OptionValues with WsScalaTestClient

