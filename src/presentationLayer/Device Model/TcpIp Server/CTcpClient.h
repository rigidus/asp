/*
 * TcpIpClient.h
 *
 *  Created on: 25 янв. 2016 г.
 *      Author: Saotomych
 */

#ifndef CTCPCLIENT_H_
#define CTCPCLIENT_H_

#include <vector>

#include <asio.hpp>
using namespace boost;

class CTcpClient
{
public:

	int32_t Connect(std::string host, int16_t port);
	void Disconnect();
	void SendMessage(std::vector<uint8_t>& data);
	int32_t RcvMessage(std::vector<uint8_t>& rcvData);
	int32_t Select(int32_t tout_sec);

};

#endif /* CTCPCLIENT_H_ */
