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
import org.scalatest.events._
import selenium.WebBrowser
import concurrent.Eventually
import concurrent.IntegrationPatience
import org.openqa.selenium.WebDriver
import BrowserDriver.NoDriver
import org.openqa.selenium.firefox.FirefoxProfile

/**
 * Trait that provides new browser instances (for all browsers available on the running platform) for each test executed in a ScalaTest <code>Suite</code>.
 *
 * It overrides ScalaTest's <code>withFixture</code>, <code>runTest</code> and <code>runTests</code> method to create new <code>WebDriver</code>, <code>TestServer</code> and
 * <code>FakeApplication</code> instance before executing each test.
 *
 * You can explicitly specify which <code>WebBrowser</code>(s) to run though config map's <code>"browsers"</code> key:
 *
 * <ul>
 *   <li>C - Chrome</li>
 *   <li>F - Firefox</li>
 *   <li>I - Internet Explorer</li>
 *   <li>S - Safari</li>
 *   <li>H - HtmlUnit</li>
 * </ul>
 *
 * For example, you can pass in <code>-Dbrowsers="CF"</code> to run <code>Chrome</code> and <code>Firefox</code> only.
 *
 * If no valid web browser is specified through config map's <code>"browsers"</code> string, it will fallback to default which discover all available <code>WebDriver</code>
 * and run with them.
 */
trait AllBrowsersPerTest extends SuiteMixin with WebBrowser with Eventually with IntegrationPatience { this: Suite =>

  private var privateApp: FakeApplication = _

  /**
   * Method to create new instance of <code>FakeApplication</code>
   */
  implicit def app: FakeApplication = synchronized { privateApp }

  /**
   * The port used by the <code>TestServer</code>.  By default this will be set to the result return from
   * <code>Helpers.testServerPort</code>, user can override this to provide their own port number.
   */
  val port: Int = Helpers.testServerPort

  /**
   * Method to provide <code>FirefoxProfile</code> for creating <code>FirefoxDriver</code>, you can override this method to
   * provide a customized instance of <code>FirefoxProfile</code>
   *
   * @return an instance of <code>FirefoxProfile</code>
   */
  protected def firefoxProfile: FirefoxProfile = new FirefoxProfile

  private val webDrivers: Set[(String, () => WebDriver)] =
    Set(
      ("Chrome", () => WebDriverFactory.createChromeDriver),
      ("Firefox", () => WebDriverFactory.createFirefoxDriver(firefoxProfile)),
      ("Internet Explorer", () => WebDriverFactory.createInternetExplorerDriver),
      ("Safari", () => WebDriverFactory.createSafariDriver),
      ("HtmlUnit", () => WebDriverFactory.createHtmlUnitDriver)
    )

  private var privateWebDriver: WebDriver = _

  @volatile private var privateWebDriverFun: () => WebDriver = _
  @volatile private var privateWebDriverName: String = _

  /**
   * Implicit method to get the <code>WebDriver</code> for the current test.
   */
  implicit def webDriver: WebDriver = synchronized { privateWebDriver }

  /**
   * Override to include the <code>WebDriver</code> in suite name.
   *
   * @return <code>super.suiteName</code> + " (" + webDriverName + ")"
   */
  abstract override def suiteName: String = {
    if (privateWebDriverName == null)
      super.suiteName
    else
      super.suiteName + " (" + privateWebDriverName + ")"
  }

  /**
   * Override <code>withFixture</code> to create new instance of <code>WebDriver</code> before
   * running each test.  If there is error when creating <code>WebDriver</code>, <code>NoDriver</code>
   * will be used and all tests will be canceled automatically.  If <code>WebDirver</code> creation
   * is successful, a new instance of <code>TestServer</code> will be started for each test before they
   * are executed.
   *
   * @param test the no-arg test function to run with a fixture
   * @return the <code>Outcome</code> of the test execution
   */
  abstract override def withFixture(test: NoArgTest): Outcome =
    webDriver match {
      case NoDriver(ex) =>
        val name = test.configMap("webDriverName")
        val message = Resources("cantCreateDriver", name)
        ex match {
          case Some(e) => Canceled(message, e)
          case None => Canceled(message)
        }
      case _ =>
        Helpers.running(TestServer(port, app)) {
          super.withFixture(test)
        }
    }

  /**
   * Override <code>runTest</code> to create <code>WebDriver</code> and <code>FakeApplication</code> before executing the test, and close the
   * <code>WebDriver</code> instance after the test is run.
   *
   * @param testName the name of one test to run.
   * @param args the <code>Args</code> for this run
   * @return a <code>Status</code> object that indicates when the test started by this method has completed, and whether or not it failed .
   */
  abstract override def runTest(testName: String, args: Args): Status = {
    synchronized {
      privateApp = new FakeApplication()
      privateWebDriver = privateWebDriverFun()
    }
    try {
      val newConfigMap = args.configMap + ("app" -> privateApp) + ("port" -> port) + ("webDriver" -> webDriver)
      val newArgs = args.copy(configMap = newConfigMap)
      super.runTest(testName, newArgs)
    }
    finally {
      webDriver match {
        case NoDriver(_) => // do nothing
        case theDriver => theDriver.close()
      }
    }
  }

  private def getFilteredWebDriverSet(testName: Option[String], args: Args): Set[(String, () => WebDriver)] = {
    val availableWebDrivers: Set[(String, () => WebDriver)] =
      Set(
        ("Chrome", () => WebDriverFactory.createChromeDriver),
        ("Firefox", () => WebDriverFactory.createFirefoxDriver(firefoxProfile)),
        ("Internet Explorer", () => WebDriverFactory.createInternetExplorerDriver),
        ("Safari", () => WebDriverFactory.createSafariDriver),
        ("HtmlUnit", () => WebDriverFactory.createHtmlUnitDriver)
      )

    args.configMap.getOptional[String]("browsers") match {
      case Some("") =>
        args.reporter(AlertProvided(
          args.tracker.nextOrdinal(),
          Resources("emptyBrowsers"),
          Some(NameInfo(this.suiteName, this.suiteId, Some(this.getClass.getName), testName))
        ))
        availableWebDrivers

      case Some(browsers) =>
        val invalidChars = browsers.filter(c => !"CFISH".contains(c.toString.toUpperCase))
        if (!invalidChars.isEmpty) {
          val (resourceName, charsString) =
            if (invalidChars.length > 1) {
              val initString = invalidChars.init.map(c => "'" + c + "'").mkString(Resources("commaSpace"))
              ("invalidBrowsersChars", Resources("and", initString, "'" + invalidChars.last  + "'"))
            }
            else
              ("invalidBrowsersChar", "'" + invalidChars.head + "'")
          args.reporter(AlertProvided(
            args.tracker.nextOrdinal(),
            Resources(resourceName, charsString),
            Some(NameInfo(this.suiteName, this.suiteId, Some(this.getClass.getName), testName))
          ))
        }
        val filteredDrivers =
          availableWebDrivers.filter { case (name, webDriverFun) =>
            browsers.toUpperCase.contains(name.charAt(0))
          }

        // If no valid option, just fallback to default that uses all available browsers
        if (filteredDrivers.isEmpty)
          availableWebDrivers
        else
          filteredDrivers

      case None => availableWebDrivers
    }
  }

  /**
   * Override <code>run</code> to discover available browsers on the running platform (and filter if -Dbrowsers=... is passed in)
   * before running the tests.  <code>super.run</code> will then be called for each targeted browser to run the tests.
   *
   * @param testName the name of one test to run.
   * @param args the <code>Args</code> for this run
   * @return a <code>Status</code> object that indicates when the test started by this method has completed, and whether or not it failed .
   */
  abstract override def run(testName: Option[String], args: Args): Status = {

    if (privateWebDriverName != null) { // it is the child suite
    val newConfigMap = args.configMap + ("webDriverName" -> privateWebDriverName)
      val newArgs = args.copy(configMap = newConfigMap)
      super.run(testName, newArgs)
    }
    else {
      val filteredWebDrivers = getFilteredWebDriverSet(testName, args)
      new CompositeStatus(
        filteredWebDrivers.map { case (name, driverFun) =>
          val instance = newInstance
          instance.privateWebDriverName = name
          instance.privateWebDriverFun = driverFun
          instance.run(testName, args)
        }
      )
    }
  }

  /**
   * Construct a new instance of this <code>Suite</code>.
   *
   * <p>
   * This trait's implementation of <code>run</code> invokes this method to create
   * a new instance of this <code>Suite</code> for each discovered <code>WebDriver</code>
   * to execute tests. This trait's implementation of this method uses reflection to call
   * <code>this.getClass.newInstance</code>. This approach will succeed only if this
   * <code>Suite</code>'s class has a public, no-arg constructor. In most cases this is
   * likely to be true, because to be instantiated by ScalaTest's <code>Runner</code> a
   * <code>Suite</code> needs a public, no-arg constructor. However, this will not be true
   * of any <code>Suite</code> defined as an inner class of another class or trait, because
   * every constructor of an inner class type takes a reference to the enclosing instance.
   * In such cases, and in cases where a <code>Suite</code> class is explicitly defined without
   * a public, no-arg constructor, you will need to override this method to construct a new
   * instance of the <code>Suite</code> in some other way.
   * </p>
   *
   * <p>
   * Here's an example of how you could override <code>newInstance</code> to construct
   * a new instance of an inner class:
   * </p>
   *
   * <pre class="stHighlight">
   * import org.scalatest.Suite
   *
   * class Outer {
   *   class InnerSuite extends Suite with AllBrowsersPerTest {
   *     def testOne() {}
   *     def testTwo() {}
   *     override def newInstance = new InnerSuite
   *   }
   * }
   * </pre>
   */
  def newInstance: Suite with AllBrowsersPerTest = this.getClass.newInstance.asInstanceOf[Suite with AllBrowsersPerTest]
}
