///////////////////////////////////////////////////////////
//  CTcpSocketFactory.h
//  Implementation of the Class CTcpSocketFactory
//  Created on:      20-���-2016 16:20:20
//  Original author: user-PC
///////////////////////////////////////////////////////////

#if !defined(EA_20F758ED_4A53_4b85_A6B8_617337A99CDF__INCLUDED_)
#define EA_20F758ED_4A53_4b85_A6B8_617337A99CDF__INCLUDED_

#include <thread/thread.hpp>
#include <thread/mutex.hpp>
using namespace boost;

class CTcpSocketFactory
{

public:
	virtual ~CTcpSocketFactory();
	CTcpSocketFactory(const CTcpSocketFactory& theCTcpSocketFactory);

	static CTcpSocketFactory* getSocketFactory();

private:
	static CTcpSocketFactory* s_pInst;
	static mutex s_Mutex;

	CTcpSocketFactory();
	CTcpSocketFactory(CTcpSocketFactory& rhs);
	CTcpSocketFactory& operator=(CTcpSocketFactory& rhs);

};
#endif // !defined(EA_20F758ED_4A53_4b85_A6B8_617337A99CDF__INCLUDED_)
