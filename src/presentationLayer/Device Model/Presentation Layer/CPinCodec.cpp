///////////////////////////////////////////////////////////
//  CPinCodec.cpp
//  Implementation of the Class CPinCodec
//  Created on:      19-���-2016 19:58:08
//  Original author: user-PC
///////////////////////////////////////////////////////////

#include "CPinCodec.h"


CPinCodec::CPinCodec(){

}



CPinCodec::~CPinCodec(){

}



CPinCodec::CPinCodec(const CPinCodec& theCPinCodec){

}





std::vector<uint8_t> CPinCodec::decode(uint8_t* rcvDataBuf, uint32_t size){

	std::vector<uint8_t> data;

	return  data;
}


std::list<std::vector<uint8_t> > CPinCodec::encode(uint8_t* dataBuf, uint32_t size){

	std::list<std::vector<uint8_t> > datalist;
	std::vector<uint8_t>* data = new std::vector<uint8_t>;

	datalist.push_back(std::move(*data));

	return  datalist;
}
