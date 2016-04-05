#if !defined(EA_A3DE04B3_F0A2_48a6_9A9B_2E7CE44DDCF6__INCLUDED_)
#define EA_A3DE04B3_F0A2_48a6_9A9B_2E7CE44DDCF6__INCLUDED_

#include <vector>
#include <list>

#include <vector>
#include <list>

#include <boost/cstdint.hpp>

class CBaseCodec
{

public:
	CBaseCodec();
	virtual ~CBaseCodec();
	CBaseCodec(const CBaseCodec& theCBaseCodec);

	std::vector<uint8_t> decode(uint8_t* rcvDataBuf, uint32_t size);
	std::list<std::vector<uint8_t> > encode(uint8_t* dataBuf, uint32_t size);

};
#endif // !defined(EA_A3DE04B3_F0A2_48a6_9A9B_2E7CE44DDCF6__INCLUDED_)
