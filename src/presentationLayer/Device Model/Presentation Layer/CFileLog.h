///////////////////////////////////////////////////////////
//  CFileLog.h
//  Implementation of the Class CFileLog
//  Created on:      19-џэт-2016 19:58:07
//  Original author: user-PC
///////////////////////////////////////////////////////////

#if !defined(EA_85424F9E_1DF9_4162_8AED_CD04D87C5909__INCLUDED_)
#define EA_85424F9E_1DF9_4162_8AED_CD04D87C5909__INCLUDED_

public class CFileLog
{

public:
	virtual ~CFileLog();
	CFileLog(const CFileLog& theCFileLog);

	int operator <<();

private:
	CFileLog();

};
#endif // !defined(EA_85424F9E_1DF9_4162_8AED_CD04D87C5909__INCLUDED_)
