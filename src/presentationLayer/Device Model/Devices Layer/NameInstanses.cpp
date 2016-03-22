/*
 * NameInstanses.cpp
 *
 *  Created on: 5 марта 2016 г.
 *      Author: drema
 */

#include "devices/LCD_winstar16x2.h"
#include "devices/testConcreteDevice.h"
#include "devices/HttpClient.h"
#include "devices/HttpDevLayerClient.h"
#include "devices/gpioShlagbaum.h"
#include "devices/UserButton.h"
#include "devices/PRN_vkp80ii_usb.h"
#include "abstract/testAbstractDevice.h"
#include "abstract/BsnsLogic.h"
#include "abstract/ShlagbaumAbstract.h"
#include "abstract/UserButtonAbstract.h"
#include "abstract/PrinterAbstract.h"
#include "abstract/DisplayAbstract.h"

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
const std::string AbstractUserButton::s_abstractName = "user-button";
const std::string AbstractDisplay::s_abstractName = "display";
const std::string AbstractPrinter::s_abstractName = "printer";

// concrete names
const std::string CTestConcreteDevice::s_concreteName = "concrete_device";
const std::string CGPIOShlagbaum::s_concreteName = "shlagbaum_gpio";
const std::string CUserButton::s_concreteName = "user-button";
const std::string HttpClient::s_concreteName = "logic_http";
const std::string HttpDevLayerClient::s_concreteName = "logic_http_dev_layer";
const std::string CSLCDWinstar16x2::s_concreteName = "symbol_LCD_winstar_16x2";
const std::string CPRN_vkp80ii_usb::s_concreteName = "printer_vkp80ii_usb";

