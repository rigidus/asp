///////////////////////////////////////////////////////////
//  CPinCtl.cpp
//  Implementation of the Class CPinCtl
//  Created on:      19-���-2016 19:58:08
//  Original author: user-PC
///////////////////////////////////////////////////////////

#include "CPinCtl.h"

CBaseCommCtl* CPinCtl::getPintCtl(uint32_t pinNum)
{
	static CPinCtl* ptr = nullptr;

	if (ptr == nullptr)
	{
		// try open files for pin
			// OK! open all files
			// create CPinCtl for num
	}

	return ptr;
}

CPinCtl::CPinCtl(){

}



CPinCtl::~CPinCtl(){

}



CPinCtl::CPinCtl(const CPinCtl& theCPinCtl){

}


bool CPinCtl::receive(int rcvData){

	return false;
}


uint32_t CPinCtl::send(std::list<std::vector<uint8_t> > sendData){

	return  0;
}


int CPinCtl::setSettings(std::string deviceName){

	return 0;
}
