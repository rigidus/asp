///////////////////////////////////////////////////////////
//  CTcpSocketFactory.h
//  Implementation of the Class CTcpSocketFactory
//  Created on:      20-џэт-2016 16:20:20
//  Original author: user-PC
///////////////////////////////////////////////////////////

#if !defined(EA_20F758ED_4A53_4b85_A6B8_617337A99CDF__INCLUDED_)
#define EA_20F758ED_4A53_4b85_A6B8_617337A99CDF__INCLUDED_

#include "CTcpServerManager.h"

public class CTcpSocketFactory
{

public:
	CTcpSocketFactory();
	virtual ~CTcpSocketFactory();
	CTcpSocketFactory(const CTcpSocketFactory& theCTcpSocketFactory);

	static CSocketFactory getSocketFactory();

private:
	static CSocketFactory* s_pInst;
	static boost::mutex s_Mutex;
	CTcpServerManager *m_CTcpServerManager;

	CSocketFactory();
	CSocketFactory(CSocketFactory& rhs);
	CSocketFactory& operator=(CSocketFactory& rhs);

};
#endif // !defined(EA_20F758ED_4A53_4b85_A6B8_617337A99CDF__INCLUDED_)
