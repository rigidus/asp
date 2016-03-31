/*
 * Logger.cpp
 *
 *  Created on: 31 марта 2016 г.
 *      Author: alex
 */

#include "Logger.h"

namespace
{
	src::severity_logger_mt< severity_level > lg;

	severity_level filterLevel = trace;
	int logSize = 1024*1024;
	std::string logFileName = "release_%2N_%d.%m.%Y-%H:%M:%S.log";

}

std::ostream& operator<< (std::ostream& strm, severity_level level)
{
	static const char* strings[] =
	{
		"trace",
		"debug",
		"warning",
		"error",
		"info",
		"critical"
	};

	if (static_cast< std::size_t >(level) < sizeof(strings) / sizeof(*strings))
		strm << strings[level];
	else
		strm << (int)level;

	return strm;
}

void Logger::init(severity_level level)
{

	boost::log::core::get()->set_filter(severity >= level);

}

void Logger::initDebugLogging()
{
    boost::shared_ptr< logging::core > core = logging::core::get();

    boost::shared_ptr< sinks::text_multifile_backend > backend =
		boost::make_shared< sinks::text_multifile_backend >();

    // Set up the file naming pattern
    backend->set_file_name_composer
    (
        sinks::file::as_file_name_composer(expr::stream << "logs/"
        		<< expr::attr< std::string >("RequestID")
				<< expr::format_date_time< boost::posix_time::ptime >("TimeStamp", "_%d-%m-%Y_%H:%M:%S")
				<< ".log"
				)
    );

    // Wrap it into the frontend and register in the core.
    // The backend requires synchronization in the frontend.
    typedef sinks::synchronous_sink< sinks::text_multifile_backend > sink_t;
    boost::shared_ptr< sink_t > sink(new sink_t(backend));

    // Set the formatter
    sink->set_formatter
    (
        expr::stream
            << "[Log: " << expr::attr< std::string >("RequestID") << "] "
			<< std::setw(8) << std::setfill('0') << line_id << std::setfill(' ') << ": "
			<< expr::format_date_time< boost::posix_time::ptime >("TimeStamp", "%d-%m-%Y %H:%M:%S.%f")
			<< ": <" << severity << ">: "
			<< expr::smessage
    );

    core->add_sink(sink);

	init(filterLevel);
	logging::add_common_attributes();
}

void Logger::initReleaseLogging()
{
	logging::add_file_log(
			keywords::target = "logs",
			keywords::file_name = logFileName,
			keywords::rotation_size = logSize * 1024,
			keywords::time_based_rotation = sinks::file::rotation_at_time_point(boost::gregorian::greg_day(1)),
			keywords::format = (
			        expr::stream
						<< std::setw(8) << std::setfill('0') << line_id << std::setfill(' ') << ": "
						<< expr::format_date_time< boost::posix_time::ptime >("TimeStamp", "%d-%m-%Y %H:%M:%S")
						<< ": <" << severity << ">: "
						<< expr::smessage
			    ),
			keywords::auto_flush = true
	);

	init(filterLevel);
	logging::add_common_attributes();
}


src::severity_logger_mt< severity_level >& Logger::getMyLogger()
{
	lg.add_attribute("LineCounter", attrs::counter< unsigned int >());

	return lg;
}

inline void Logger::CommonLog(severity_level level, std::string message)
{
	src::severity_logger_mt< severity_level >& lg = getMyLogger();
	BOOST_LOG_SEV(lg, level) << message;
}

inline void Logger::LocalLog(std::string name, severity_level level, std::string message)
{
	BOOST_LOG_SCOPED_THREAD_TAG("RequestID", name);
	src::severity_logger_mt< severity_level >& lg = getMyLogger();

    BOOST_LOG_SEV(lg, level) << message;
}


void Logger::SetToCommonLog(severity_level level, std::string message)
{
	std::cout << " lineid = " << line_id << std::endl;

#ifdef NDEBUG
	CommonLog(level, message);
#else
	LocalLog("debug", level, message);
#endif
}

void Logger::SetToLocalLog(std::string name, severity_level level, std::string message)
{
	std::cout << " lineid = " << line_id << std::endl;

#ifndef NDEBUG
	LocalLog(name, level, message);
#endif
   	SetToCommonLog(level, message);
}

template<>
boost::log::trivial::severity_level Logger::getArg<boost::log::trivial::severity_level> (std::string str)
{
	if (str == "trace") return boost::log::trivial::trace;
	if (str == "debug") return boost::log::trivial::debug;
	if (str == "info") return boost::log::trivial::info;
	if (str == "warning") return boost::log::trivial::warning;
	if (str == "error") return boost::log::trivial::error;
	if (str == "fatal") return boost::log::trivial::fatal;
	return boost::log::trivial::trace;
}

template<>
severity_level Logger::getArg<severity_level> (std::string str)
{
	if (str == "trace") return severity_level::trace;
	if (str == "debug") return severity_level::debug;
	if (str == "warning") return severity_level::warning;
	if (str == "error") return severity_level::error;
	if (str == "info") return severity_level::info;
	if (str == "critical") return severity_level::critical;
	return severity_level::trace;
}

int32_t Logger::LogParamParser(char* argv, char* argp)
{
	std::string argval(argv);
	int32_t len = 0;

	if ( argval == "--loglevel" || argval == "-l" )
	{
		filterLevel = getArg<severity_level>(argp);
		len=2;
	}

	if ( argval == "--logsize" || argval == "-s" )
	{
		logSize = getArg<ToIntegerConvertor>(argp).v();
		len=2;
	}

	if ( argval == "--logname" || argval == "-f" )
	{
		logFileName = getArg<std::string>(argp);
		len=2;
	}

	return len;
}
