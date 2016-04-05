//============================================================================
// Name        : SeverityLevels.h
// Author      : aav
// Created on  : 31 марта 2016 г.
// Version     : v.0.1
// Copyright   : Non nobis, Domine, non nobis, sed nomini tuo da gloriam.
// Description : Levels for file logger based by boost library libboost_log
//============================================================================

#ifndef SEVERITYLEVELS_H_
#define SEVERITYLEVELS_H_

#include <iostream>

// We define our own severity levels
enum severity_level
{
    trace,
	debug,
    warning,
    error,
	info,
    critical,
	last
};

// The operator puts a human-friendly representation of the severity level to the stream
std::ostream& operator<< (std::ostream& strm, severity_level level);


#endif /* SEVERITYLEVELS_H_ */
