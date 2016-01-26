/*
 * TcpIpClient.cpp
 *
 *  Created on: 25 янв. 2016 г.
 *      Author: Saotomych
 */

#include "CTcpClient.h"

int32_t CTcpClient::Connect(std::string host, int16_t port)
{

	return 0;
}


void CTcpClient::Disconnect()
{

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


