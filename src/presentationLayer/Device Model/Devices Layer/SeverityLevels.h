/*
 * SeverityLevels.h
 *
 *  Created on: 31 марта 2016 г.
 *      Author: alex
 */

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
