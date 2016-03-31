/*
 * Logger.h
 *
 *  Created on: 31 марта 2016 г.
 *      Author: alex
 */

#ifndef LOGGER_H_
#define LOGGER_H_

#define BOOST_LOG_DYN_LINK 1

#include <iostream>
#include <string>
#include <iomanip>

#include <boost/log/core.hpp>
#include <boost/log/trivial.hpp>
#include <boost/log/expressions.hpp>

#include <boost/log/sinks/sync_frontend.hpp>
#include <boost/log/sinks/text_file_backend.hpp>
#include <boost/log/sinks/text_multifile_backend.hpp>

#include <boost/log/utility/setup/file.hpp>
#include <boost/log/utility/setup/common_attributes.hpp>

#include <boost/log/sources/severity_logger.hpp>
#include <boost/log/sources/record_ostream.hpp>
#include <boost/log/sources/logger.hpp>

#include <boost/log/expressions.hpp>
#include <boost/log/expressions/attr_fwd.hpp>
#include <boost/log/expressions/attr.hpp>

#include <boost/log/attributes/attribute.hpp>
#include <boost/log/attributes/attribute_cast.hpp>
#include <boost/log/attributes/attribute_value.hpp>
#include <boost/log/attributes/scoped_attribute.hpp>
#include <boost/log/attributes/counter.hpp>

#include <boost/log/support/date_time.hpp>

#include <boost/smart_ptr/shared_ptr.hpp>
#include <boost/smart_ptr/make_shared_object.hpp>

#include <boost/date_time/posix_time/posix_time_types.hpp>
#include <boost/date_time/gregorian/gregorian.hpp>

#include "SeverityLevels.h"

namespace logging = boost::log;
namespace trivial = logging::trivial;
namespace src = boost::log::sources;
namespace sinks = boost::log::sinks;
namespace keywords = boost::log::keywords;
namespace expr = boost::log::expressions;
namespace attrs = boost::log::attributes;
namespace gregorian = boost::gregorian;


namespace Logger
{
	using namespace std;

	BOOST_LOG_ATTRIBUTE_KEYWORD(line_id, "LineID", unsigned int)
	BOOST_LOG_ATTRIBUTE_KEYWORD(severity, "Severity", severity_level)

	void init(severity_level level);

	class ToIntegerConvertor
	{
		int value;

	public:

		ToIntegerConvertor(): value(0) {}
		ToIntegerConvertor(int v): value(v) {}
		ToIntegerConvertor( const std::string& op2 )
		{
			value = stoi(op2);
		}

		ToIntegerConvertor& operator=( const std::string& op2 )
		{
			value = stoi(op2);
			return *this;
		}

		int v()
		{
			return value;
		}

	};

	void initDebugLogging();

	void initReleaseLogging();

	src::severity_logger_mt< severity_level >& getMyLogger();
	inline void CommonLog(severity_level level, std::string message);
	inline void LocalLog(std::string name, severity_level level, std::string message);
	void SetToCommonLog(severity_level level, std::string message);
	void SetToLocalLog(std::string name, severity_level level, std::string message);

	template<typename T>
	T getArg(std::string str)
	{
		T res(str);
		return res;
	}

	template<>
	boost::log::trivial::severity_level getArg<boost::log::trivial::severity_level> (std::string str);

	template<>
	severity_level getArg<severity_level> (std::string str);

	int32_t LogParamParser(char* argv, char* argp);

}



#endif /* LOGGER_H_ */
