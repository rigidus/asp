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

class CTcpClient
{
public:

	CTcpClient();

	int32_t Connect(std::string host, int16_t port);
	void Disconnect();
	void SendMessage(std::vector<uint8_t>& data);
	int32_t RcvMessage(std::vector<uint8_t>& rcvData);
	int32_t Select(int32_t tout_sec);

private:

	asio::io_service m_ioService;
	asio::ip::tcp::socket m_socket;
	bool m_stopped;
	asio::streambuf m_inputBuffer;
	asio::deadline_timer m_deadline;
	asio::deadline_timer m_heartbeatTimer;

	void startConnect(tcp::resolver::iterator endpointIter);
	void handle_connect(const boost::system::error_code& ec,
	  tcp::resolver::iterator endpointIter);
	void start_read();
	void handle_read(const boost::system::error_code& ec);
	void start_write();
	void handle_write(const boost::system::error_code& ec);
	void check_deadline();
};

#endif /* CTCPCLIENT_H_ */
