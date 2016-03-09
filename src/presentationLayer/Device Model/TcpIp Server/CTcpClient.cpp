/*
 * TcpIpClient.cpp
 *
 *  Created on: 25 янв. 2016 г.
 *      Author: Saotomych
 */

#include "CTcpClient.h"

CTcpClient::CTcpClient(std::string remoteHost, uint32_t remotePort)
: io_service(),
  stopped_(false),
  socket_(io_service),
  deadline_(io_service),
  heartbeat_timer_(io_service),
  host(remoteHost),
  port(remotePort),
  was_read(true),
  was_write(true)
{

	CFileLog::cfilelog() << "CTcpClient created" << std::endl;
}

CTcpClient::~CTcpClient()
{
	CFileLog::cfilelog() << "CTcpClient deleted" << std::endl;
}

void CTcpClient::start(tcp::resolver::iterator endpoint_iter)
{
	CFileLog::cfilelog() << "CTcpClient::start" << std::endl;

	// Start the connect actor.
	start_connect(endpoint_iter);

	// Start the deadline actor. You will note that we're not setting any
	// particular deadline here. Instead, the connect and input actors will
	// update the deadline prior to each asynchronous operation.
	deadline_.expires_from_now(boost::posix_time::seconds(10));
	deadline_.async_wait(boost::bind(&CTcpClient::check_deadline, this, _1));
}

void CTcpClient::stop()
{
	CFileLog::cfilelog() << "CTcpClient::stop" << std::endl;

	stopped_ = true;
	deadline_.cancel();
}

void CTcpClient::handle_stop()
{
	CFileLog::cfilelog() << "CTcpClient::handle_stop" << std::endl;

	stopped_ = true;
	socket_.close();
	deadline_.expires_from_now(boost::posix_time::seconds(0));
	//heartbeat_timer_.cancel();
}

bool CTcpClient::is_connected()
{
	return socket_.is_open();
}

void CTcpClient::start_connect(tcp::resolver::iterator endpoint_iter)
{
	CFileLog::cfilelog() << "CTcpClient::start_connect" << std::endl;

	if (endpoint_iter != tcp::resolver::iterator())
	{
		std::cout << "Trying " << endpoint_iter->endpoint() << "...\n";

		// Start the asynchronous connect operation.
		socket_.async_connect(endpoint_iter->endpoint(),
		  boost::bind(&CTcpClient::handle_connect,
			this, _1, endpoint_iter));

	}
	else
	{
		// There are no more endpoints to try. Shut down the client.
		stop();
	}
}

void CTcpClient::handle_connect(const boost::system::error_code& ec,
		tcp::resolver::iterator endpoint_iter)
{
	CFileLog::cfilelog() << "CTcpClient::handle_connect with error_code=" << ec.message() << std::endl;

	if (stopped_)
	{
		handle_stop();
		return;
	}

	// The async_connect() function automatically opens the socket at the start
	// of the asynchronous operation. If the socket is closed at this time then
	// the timeout handler must have run first.
	if (!socket_.is_open())
	{
		CFileLog::cfilelog() << "Connect timed out\n" << std::endl;

		// Try the next available endpoint.
		start_connect(++endpoint_iter);
	}

	// Check if the connect operation failed before the deadline expired.
	else if (ec)
	{
		CFileLog::cfilelog() << "Connect error: " << ec.message() << std::endl;

		// We need to close the socket used in the previous connection attempt
		// before starting a new one.
		handle_stop();
	}

	// Otherwise we have successfully established a connection.
	else
	{
		CFileLog::cfilelog() << "Connected to " << endpoint_iter->endpoint() << std::endl;

		// Start the input actor.
		start_read();

		// For another applications with the heartbeat
		// Start the heartbeat actor.
//		start_write();
	}
}

void CTcpClient::start_read()
{
	CFileLog::cfilelog() << "CTcpClient::start_read" << std::endl;

	// Start an asynchronous operation to read a newline-delimited message.
	boost::asio::async_read_until(socket_, input_buffer_, '\n',
			boost::bind(&CTcpClient::handle_read, this, _1));

}

void CTcpClient::handle_read(const boost::system::error_code& ec)
{
	CFileLog::cfilelog() << "CTcpClient::handle_read: error_code=" << ec.message() << std::endl;

	if (stopped_)
	{
		handle_stop();
		return;
	}

	if (!ec)
	{
		was_read = true;

		// Extract the newline-delimited message from the buffer.
		std::istream is(&input_buffer_);

		// TODO: call callback to client software

		start_read();
	}
	else
	{
		std::cout << "Error on receive: " << ec.message() << "\n";

		handle_stop();
	}
}

void CTcpClient::start_write()
{
	CFileLog::cfilelog() << "CTcpClient::start_write" << std::endl;

	if (stopped_)
	{
		handle_stop();
		return;
	}

	// Start an asynchronous operation to send a heartbeat message.
	boost::asio::async_write(socket_, boost::asio::buffer("\n", 1),
			boost::bind(&CTcpClient::handle_write, this, _1));
}

void CTcpClient::send_message(std::vector<uint8_t>& data)
{
	CFileLog::cfilelog() << "CTcpClient::send_message" << std::endl;

	if (stopped_)
	{
		handle_stop();
		return;
	}

	// Start an asynchronous operation to send a data message.
	boost::asio::async_write(socket_, boost::asio::buffer(&data[0], data.size()),
			boost::bind(&CTcpClient::handle_write, this, _1));
}

void CTcpClient::handle_write(const boost::system::error_code& ec)
{
	CFileLog::cfilelog() << "CTcpClient::handle_write with error_code=" << ec << std::endl;

	if (stopped_)
	{
		handle_stop();
		return;
	}

	if (!ec)
	{
		was_write = true;
		// Heartbeat off here client
		// Wait 10 seconds before sending the next heartbeat.
//		heartbeat_timer_.expires_from_now(boost::posix_time::seconds(10));

		// It's the heartbeat frame
//		heartbeat_timer_.async_wait(boost::bind(&CTcpClient::start_write, this));
	}
	else
	{
		CFileLog::cfilelog() << "Error on heartbeat: " << ec.message() << std::endl;

		handle_stop();
	}
}

void CTcpClient::check_deadline(const boost::system::error_code& ec)
{
	CFileLog::cfilelog() << "CTcpClient::check_deadline: " << ec.message() << std::endl;

	// Check whether the deadline has passed. We compare the deadline against
	// the current time since a new asynchronous operation may have moved the
	// deadline before this actor had a chance to run.
//	if (m_deadline.expires_at() <= boost::asio::deadline_timer::traits_type::now())
	if (stopped_)
	{
		handle_stop();
		return;
	}

	if (ec)
		return;

	if ((was_read == false) && (was_write == false))
	{
		handle_stop();
		deadline_.expires_from_now(boost::posix_time::milliseconds(100));
		deadline_.async_wait(boost::bind(&CTcpClient::check_deadline, this, _1));
		return;
	}

	was_read = false;
	was_write = false;

	// Put the actor back to sleep.
	deadline_.expires_from_now(boost::posix_time::seconds(10));
	deadline_.async_wait(boost::bind(&CTcpClient::check_deadline, this, _1));
}

CTcpClient* CTcpClient::clientFactory(std::string host, uint32_t port)
{
	CFileLog::cfilelog() << "CTcpClient::clientFactory: " << host << ":" << port  << std::endl;

	CTcpClient* client = nullptr;
	try
	{
		client = new CTcpClient(host, port);
	}

	catch (std::exception& e)
	{
		std::cerr << "Exception: " << e.what() << "\n";
	}

	return client;
}

void CTcpClient::startClient(CTcpClient* client)
{
	CFileLog::cfilelog() << "CTcpClient::startClient" << std::endl;

	try
	{
		tcp::resolver r(client->io_service);

		std::string strport(std::to_string(client->port));
		client->start(r.resolve(tcp::resolver::query(client->host, strport)));

		client->io_service.run();

		CFileLog::cfilelog() << "CTcpClient::io_service cancelled" << std::endl;
	}
	catch (std::exception& e)
	{
		std::cerr << "Exception: " << e.what() << "\n";
	}
}
