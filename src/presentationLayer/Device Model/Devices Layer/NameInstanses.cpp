/*
 * NameInstanses.cpp
 *
 *  Created on: 5 марта 2016 г.
 *      Author: drema
 */

#include "devices/testConcreteDevice.h"
#include "devices/HttpClient.h"
#include "devices/HttpDevLayerClient.h"
#include "devices/gpioShlagbaum.h"

#include "abstract/testAbstractDevice.h"
#include "abstract/BsnsLogic.h"
#include "abstract/ShlagbaumAbstract.h"

// Abstract names
// full
//const std::string test_device("abstract_device");
//const std::string shlagbaum1("shlagbaum_in");
//const std::string shlagbaum2("shlagbaum_out");
//const std::string printer1("printer");
//const std::string photosensor1("pass_photosensor");
//const std::string photosensor2("present_photosensor");
//const std::string display1("display");
//const std::string massstorage1("sd_card");
//const std::string kkm1("kkm");

// common
const std::string CTestAbstractDevice::s_abstractName = "abstract";
const std::string AbstractShlagbaum::s_abstractName = "shlagbaum";
const std::string BsnsLogic::s_abstractName = "logic";

// concrete names
const std::string CTestConcreteDevice::s_concreteName = "concrete_device";
const std::string CGPIOShlagbaum::s_concreteName = "shlagbaum_gpio";
const std::string HttpClient::s_concreteName = "logic_http";
const std::string HttpDevLayerClient::s_concreteName = "logic_http_dev_layer";
