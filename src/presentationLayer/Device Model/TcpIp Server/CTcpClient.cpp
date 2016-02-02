/*
 * TcpIpClient.cpp
 *
 *  Created on: 25 янв. 2016 г.
 *      Author: Saotomych
 */

#include "CTcpClient.h"

CTcpClient::CTcpClient():
	m_socket(m_ioService),
	m_stopped(true),
    m_deadline(m_ioService),
    m_heartbeatTimer(m_ioService)
{

}


int32_t CTcpClient::Connect(std::string host, int16_t port)
{

	if (m_socket.is_open() == false)
	{
		tcp::resolver r(m_ioService);

		std::string sPort(std::to_string(port));

		startConnect(r.resolve(tcp::resolver::query(host, sPort)));

		if (m_ioService.stopped() == true) m_ioService.run();
	}

	return 0;
}


void CTcpClient::Disconnect()
{
    m_stopped = true;
    m_socket.close();
    m_deadline.cancel();
    m_heartbeatTimer.cancel();
}


void CTcpClient::SendMessage(std::vector<uint8_t>& data)
{

}


int32_t CTcpClient::RcvMessage(std::vector<uint8_t>& rcvData)
{
	int32_t len = 0;

	return len;
}


int32_t CTcpClient::Select(int32_t tout_sec)
{

	return 0;
}


void CTcpClient::startConnect(tcp::resolver::iterator endpointIter)
{
	if (endpointIter != tcp::resolver::iterator())
	{
	  std::cout << "Trying " << endpointIter->endpoint() << "...\n";

	  // Set a deadline for the connect operation.
	  m_deadline.expires_from_now(boost::posix_time::seconds(60));

	  // Start the asynchronous connect operation.
	  m_socket.async_connect(endpointIter->endpoint(),
		  boost::bind(&CTcpClient::handle_connect,
			this, _1, endpointIter));
	}
	else
	{
	  // There are no more endpoints to try. Shut down the client.
	  Disconnect();
	}
}

void CTcpClient::handle_connect(const boost::system::error_code& ec,
  tcp::resolver::iterator endpointIter)
{
	if (m_stopped) return;

	// The async_connect() function automatically opens the socket at the start
	// of the asynchronous operation. If the socket is closed at this time then
	// the timeout handler must have run first.
	if (!m_socket.is_open())
	{
	  std::cout << "Connect timed out\n";

	  // Try the next available endpoint.
	  startConnect(++endpointIter);
	}

	// Check if the connect operation failed before the deadline expired.
	else if (ec)
	{
	  std::cout << "Connect error: " << ec.message() << "\n";

	  // We need to close the socket used in the previous connection attempt
	  // before starting a new one.
	  m_socket.close();

	  // Try the next available endpoint.
	  startConnect(++endpointIter);
	}
	// Otherwise we have successfully established a connection.
	else
	{
	  std::cout << "Connected to " << endpointIter->endpoint() << "\n";

	  // Start the input actor.
	  start_read();

	  // Start the heartbeat actor.
	  start_write();
	}
}


void CTcpClient::start_read()
{
	// Set a deadline for the read operation.
	m_deadline.expires_from_now(boost::posix_time::seconds(30));

	// Start an asynchronous operation to read a newline-delimited message.
	boost::asio::async_read_until(m_socket, m_inputBuffer, '\n',
		boost::bind(&CTcpClient::handle_read, this, _1));
}

void CTcpClient::handle_read(const boost::system::error_code& ec)
{
	if (m_stopped)
	  return;

	if (!ec)
	{
	  // Extract the newline-delimited message from the buffer.
	  std::string line;
	  std::istream is(&m_inputBuffer);
	  std::getline(is, line);

	  // Empty messages are heartbeats and so ignored.
	  if (!line.empty())
	  {
		std::cout << "Received: " << line << "\n";
	  }

	  start_read();
	}
	else
	{
	  std::cout << "Error on receive: " << ec.message() << "\n";

	  Disconnect();
	}
}


void CTcpClient::start_write()
{
	if (m_stopped) return;

	// Start an asynchronous operation to send a heartbeat message.
	boost::asio::async_write(m_socket, boost::asio::buffer("\n", 1),
		boost::bind(&CTcpClient::handle_write, this, _1));
}

void CTcpClient::handle_write(const boost::system::error_code& ec)
{
	if (m_stopped)
	  return;

	if (!ec)
	{
	  // Wait 10 seconds before sending the next heartbeat.
	  m_heartbeatTimer.expires_from_now(boost::posix_time::seconds(10));
	  m_heartbeatTimer.async_wait(boost::bind(&CTcpClient::start_write, this));
	}
	else
	{
	  std::cout << "Error on heartbeat: " << ec.message() << "\n";

	  Disconnect();
	}
}


void CTcpClient::check_deadline()
{
	if (m_stopped) return;

	// Check whether the deadline has passed. We compare the deadline against
	// the current time since a new asynchronous operation may have moved the
	// deadline before this actor had a chance to run.
	if (m_deadline.expires_at() <= boost::asio::deadline_timer::traits_type::now())
	{
	  // The deadline has passed. The socket is closed so that any outstanding
	  // asynchronous operations are cancelled.
	  m_socket.close();

	  // There is no longer an active deadline. The expiry is set to positive
	  // infinity so that the actor takes no action until a new deadline is set.
	  m_deadline.expires_at(boost::posix_time::pos_infin);
	}

	// Put the actor back to sleep.
	m_deadline.async_wait(boost::bind(&CTcpClient::check_deadline, this));
}
