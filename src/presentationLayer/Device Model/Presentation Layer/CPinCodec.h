///////////////////////////////////////////////////////////
//  CPinCodec.h
//  Implementation of the Class CPinCodec
//  Created on:      19-џэт-2016 19:58:08
//  Original author: user-PC
///////////////////////////////////////////////////////////

#if !defined(EA_B48E02C6_BE7B_44f4_93A5_0224FD4BA094__INCLUDED_)
#define EA_B48E02C6_BE7B_44f4_93A5_0224FD4BA094__INCLUDED_

#include "CBaseCodec.h"

public class CPinCodec : private CBaseCodec
{

public:
	CPinCodec();
	virtual ~CPinCodec();
	CPinCodec(const CPinCodec& theCPinCodec);

	std::vector<uint8_t> decode(uint8_t* rcvDataBuf, uint32_t size);
	std::list<std::vector<uint8_t> > encode(uint8_t* dataBuf, uint32_t size);

};
#endif // !defined(EA_B48E02C6_BE7B_44f4_93A5_0224FD4BA094__INCLUDED_)
