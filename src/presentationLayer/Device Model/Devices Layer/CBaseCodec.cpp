#include "CBaseCodec.h"


CBaseCodec::CBaseCodec(){

}



CBaseCodec::~CBaseCodec(){

}



CBaseCodec::CBaseCodec(const CBaseCodec& theCBaseCodec){

}


std::vector<uint8_t> CBaseCodec::decode(uint8_t* rcvDataBuf, uint32_t size){

	std::vector<uint8_t> data;

	return  data;
}


std::list<std::vector<uint8_t> > CBaseCodec::encode(uint8_t* dataBuf, uint32_t size){

	std::list<std::vector<uint8_t> > datalist;
	std::vector<uint8_t> data;

	datalist.push_back(std::move(data));

	return  datalist;
}
