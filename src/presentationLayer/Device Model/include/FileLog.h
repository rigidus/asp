#ifndef DEBUG_LOG
#define DEBUG_LOG

// TODO: delete DEBUG define
#define DEBUG	// temporary
#define LOGTOFILE

#include <iostream>
#include <fstream>
#include <string>
#include <thread.hpp>
#include <thread/mutex.hpp>
#include <boost/date_time/posix_time/posix_time.hpp>

class CFileLog;

namespace StreamStdStrings
{
	template <typename T>
	void toStream(const T& p, std::ostream* fLog, boost::mutex& tmut, boost::posix_time::ptime& bgntime, bool& ts)
	{
		boost::mutex::scoped_lock lock(tmut);

		if (ts)
		{
			ts = false;

			boost::posix_time::ptime t(boost::posix_time::microsec_clock::local_time());
#ifdef LOGTOFILE
			*fLog << (t-bgntime) << ": ";
#else
			std::cerr << (t-bgntime) << ": ";
#endif
		}
#ifdef LOGTOFILE
		*fLog << p;
#else
		std::cerr << p;
#endif
	}

//	template <>
//	void toStream<std::string>(const std::string& p, std::ostream* fLog, boost::mutex& tmut, boost::posix_time::ptime& bgntime, bool& ts)
//	{
//		boost::mutex::scoped_lock lock(tmut);
//
//		if (ts)
//		{
//			ts = false;
//
//			boost::posix_time::ptime t(boost::posix_time::microsec_clock::local_time());
//#ifdef LOGTOFILE
//			*fLog << (t-bgntime) << ": ";
//#else
//			std::cerr << (t-bgntime) << ": ";
//#endif
//		}
//
//		for (uint32_t i = 0; i < p.size(); i++)
//		{
//#ifdef LOGTOFILE
//			*fLog << p.c_str()[i];
//#else
//			std::cerr << p.c_str()[i];
//#endif
//		}
//	}
//
//
//	template <>
//	void toStream<std::wstring>(const std::wstring& p, std::ostream* fLog, boost::mutex& tmut, boost::posix_time::ptime& bgntime, bool& ts)
//	{
//		boost::mutex::scoped_lock lock(tmut);
//
//		if (ts)
//		{
//			ts = false;
//
//			boost::posix_time::ptime t(boost::posix_time::microsec_clock::local_time());
//#ifdef LOGTOFILE
//			*fLog << (t-bgntime) << ": ";
//#else
//			std::cerr << (t-bgntime) << ": ";
//#endif
//		}
//
//		for (uint32_t i = 0; i < p.size(); i++)
//		{
//#ifdef LOGTOFILE
//			*fLog << p.c_str()[i];
//#else
//			std::cerr << p.c_str()[i];
//#endif
//		}
//	}

}

class CFileLog
{
	bool ts;
	boost::posix_time::ptime bgntime;
	boost::mutex mut;
	boost::mutex tmut;

#ifdef LOGTOFILE
	std::filebuf fBuf;
	std::ostream* fLog;
#endif
	
	CFileLog(): ts(true), bgntime(boost::posix_time::microsec_clock::local_time()) 
	{
#ifdef LOGTOFILE
		fBuf.open("project.log", std::ios::out);
		fLog = new std::ostream(&fBuf);

		*fLog << std::endl;
		*fLog << "--- Start new adaptation ---" << std::endl;
#endif
	}

public:

	static CFileLog& cfilelog()
	{
		static CFileLog *l=NULL;
		static boost::mutex mut;

		boost::mutex::scoped_lock lock(mut);

		if (!l) l = new CFileLog();
		l->locklog();
		return *l;
	}

	CFileLog& locklog() {

#ifdef DEBUG
		mut.lock();
#endif
		return *this; 
	}

	typedef std::basic_ostream<char, std::char_traits<char> > CoutType;
	typedef CoutType& (*StandardEndLine)(CoutType&);
	
#ifdef DEBUG
	template <typename T>
	CFileLog& operator <<(const T& p)
	{
		StreamStdStrings::toStream(p, fLog, tmut, bgntime, ts);

		return *this;
	}


	CFileLog& operator <<(StandardEndLine e)
    {
		boost::mutex::scoped_lock lock(tmut);

#ifdef LOGTOFILE
		*fLog << std::endl;
		fLog->flush();
#else
		std::cerr << std::endl;
		std::cerr.flush();
#endif // LOGTOFILE

		ts = true;
		mut.unlock();

		return *this;
    }
#else

	template <typename T>
	CFileLog& operator <<(const T& p)
	{
		return *this;
	}

	CFileLog& operator <<(StandardEndLine e)
    {
		return *this;
    }
#endif
	
};

#endif // DEBUG_LOG

