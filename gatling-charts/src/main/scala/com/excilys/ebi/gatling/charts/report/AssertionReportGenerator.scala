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
package com.excilys.ebi.gatling.charts.report

import com.excilys.ebi.gatling.charts.config.ChartsFiles._
import com.excilys.ebi.gatling.charts.template.{JunitAssertionTemplate,TsvAssertionTemplate}
import com.excilys.ebi.gatling.core.config.GatlingConfiguration.configuration
import com.excilys.ebi.gatling.core.result.message.RequestStatus.{ KO, OK }
import com.excilys.ebi.gatling.core.result.reader.{GeneralStats, DataReader}
import scala._
import com.excilys.ebi.gatling.core.structure.Assertion
import scala.collection.mutable.LinkedHashSet
import com.excilys.ebi.gatling.core.result.Group
import scala.Some
import grizzled.slf4j.Logging


class TestSuite(scenarioName:String, testCaseList:LinkedHashSet[TestCase]){

	private def accFailedCount(count:Int,testcase:TestCase):Int ={
		if (testcase.tcstatus) {
         count
		} else {
         count + 1
		}
  }

	def tsname = scenarioName

	def tcList = testCaseList

	def failureCount = testCaseList.foldLeft[Int](0)(accFailedCount)

}

class TestCase(msg:String,name:String,status:String,actualNumber:String,assertionItem:String,requestName:String,assertionNumber:String) {

	def tcname = name

	def tcstatus = status.toBoolean

	def tcsysout = msg + " - Actual Number " + actualNumber

	def tctype = assertionItem

	def tcrequestname = requestName

	def tcactualnumber = actualNumber

	def tcassertionnumber = assertionNumber

}

class AssertionReportGenerator(runOn: String, dataReader: DataReader, assertions: Seq[Assertion]) extends Logging {

	def generateTestCase(testcases:LinkedHashSet[TestCase],assertion:Assertion) : LinkedHashSet[TestCase] ={

		val message = assertion.assertionMessage
		val percent1 = configuration.charting.indicators.percentile1
		val percent2 = configuration.charting.indicators.percentile2

		val assertionPercentile1:String = percent1 + "th percentile response time"
		val assertionPercentile2:String = percent2 + "th percentile response time"
		val assertionThroughput:String = "requests per second"
		val assertionPercentageKO:String = "percentage of requests KO"
		val assertionMean = "mean response time"
		val assertionMin = "min response time"
		val assertionMax = "max response time"
		val assertionStv = "standard deviation response time"

		val msgPattern = """(.*) (%s|%s|%s|%s|%s|%s|%s|%s) is (greater|less) than (\d{1,5}) : (false|true)""".stripMargin.format(assertionPercentile1,assertionPercentile2,assertionThroughput,assertionPercentageKO,assertionMean,assertionMin,assertionMax,assertionStv).r
		val nonMatchPattern = """(.*) : (false|true)""".r

 		def generateActualNumber(requestName: Option[String], group: Option[Group], assertionItem:String) : String = {

			val total = dataReader.generalStats(None, requestName, group)
			val ok = dataReader.generalStats(Some(OK), requestName, group)

			assertionItem match {
				case `assertionPercentile1` => total.percentile1.toString
				case `assertionPercentile2` => total.percentile2.toString
				case `assertionThroughput` => total.meanRequestsPerSec.toString
				case `assertionPercentageKO` => if (total.count != 0) "%.2f".format(((total.count - ok.count).toFloat/total.count)*100) else ""
				case `assertionMean` => total.mean.toString
				case `assertionMin` => total.min.toString
				case `assertionMax` => total.max.toString
				case `assertionStv` => total.stdDev.toString
				case _ => "";
      }

    }

		val reqPattern = """(.*) / (.*)""".r
		message match {
					case msgPattern(requestName,assertionItem, assertionOperation, assertionNumber, status) => {
							val tcname = "%s %s is %s than %s".stripMargin.format(requestName,assertionItem,assertionOperation,assertionNumber)
							requestName match {
									case "Global" => testcases += new TestCase(message,tcname,status,generateActualNumber(None,None,assertionItem),assertionItem,requestName,assertionNumber)
									case reqPattern(groupName,reqName) => testcases += new TestCase(message,tcname,status,generateActualNumber(Some(reqName),Some(new Group(groupName)),assertionItem),assertionItem,requestName,assertionNumber)
									case _ => testcases += new TestCase(message,tcname,status,generateActualNumber(Some(requestName),None,assertionItem),assertionItem,requestName,assertionNumber)
							}
					}
					case nonMatchPattern(tcname, status) => {
							testcases += new TestCase(message,tcname,status,"N/A","N/A","N/A","N/A")
					}
		}
  }

		def generate{
			try{
					val testCases = assertions.foldLeft[LinkedHashSet[TestCase]](LinkedHashSet[TestCase]())(generateTestCase)
					val testsuite = new TestSuite(dataReader.scenarioNames.mkString,testCases)
					new TemplateWriter(jUnitAssertionFile).writeToFile(new JunitAssertionTemplate(testsuite).getOutput)
					new TemplateWriter(tsvAssertionFile(runOn)).writeToFile(new TsvAssertionTemplate(testsuite).getOutput)
			}catch{
					case e: Exception => logger.error("Error occurred during assertion report generation", e)
			}
  	}

}
