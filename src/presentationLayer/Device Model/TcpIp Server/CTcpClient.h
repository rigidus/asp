/*
 * TcpIpClient.h
 *
 *  Created on: 25 янв. 2016 г.
 *      Author: Saotomych
 */

#ifndef CTCPCLIENT_H_
#define CTCPCLIENT_H_

#include <string>
#include <vector>
#include <iostream>

#include <boost/asio/deadline_timer.hpp>
#include <boost/asio/io_service.hpp>
#include <boost/asio/ip/tcp.hpp>
#include <boost/asio/read_until.hpp>
#include <boost/asio/streambuf.hpp>
#include <boost/asio/write.hpp>
#include <boost/bind.hpp>
#include <asio.hpp>
using namespace boost;
using namespace boost::asio::ip;

#include "../include/FileLog.h"

class CTcpClient
{
public:

	CTcpClient(std::string remoteHost, uint32_t remotePort);

	~CTcpClient();

	// Called by the user of the client class to initiate the connection process.
	// The endpoint iterator will have been obtained using a tcp::resolver.
	void start(tcp::resolver::iterator endpoint_iter);

	// This function terminates all the actors to shut down the connection. It
	// may be called by the user of the client class, or by the class itself in
	// response to graceful termination or an unrecoverable error.
	void stop();

	void send_message(std::vector<uint8_t>& data);

	bool is_connected();

private:
	void start_connect(tcp::resolver::iterator endpoint_iter);
	void handle_connect(const boost::system::error_code& ec,
	  tcp::resolver::iterator endpoint_iter);
	void start_read();
	void handle_read(const boost::system::error_code& ec);
	void start_write();
	void handle_write(const boost::system::error_code& ec);
	void check_deadline(const boost::system::error_code& ec);

private:

	void handle_stop();

	boost::asio::io_service io_service;
	bool stopped_;
	tcp::socket socket_;
	boost::asio::streambuf input_buffer_;
	boost::asio::deadline_timer deadline_;
	boost::asio::deadline_timer heartbeat_timer_;
	std::string host;
	uint32_t port;
	bool was_read;
	bool was_write;

public:

    static CTcpClient* clientFactory( std::string host, uint32_t port);
    static void startClient(CTcpClient* client);
};

#endif /* CTCPCLIENT_H_ */
