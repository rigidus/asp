/*
 * clientHttp.h
 *
 *  Created on: 24 февр. 2016 г.
 *      Author: alex
 */

#ifndef HTTPCLIENT_H_
#define HTTPCLIENT_H_

class HttpClient: public CBaseDevice
{

// CodecType protoCodec;

public:

	HttpClient(): CBaseDevice(s_concreteName) {}

	~HttpClient()
	{
	}

	static const std::string s_concreteName;

	std::vector<uint8_t> rcvData;

	virtual void sendCommand(const std::string command, const std::string pars)
	{
		std::cout << "Businness logic client performs command: " << command << "[" << pars << "]" << std::endl;

		// TODO Сериализация JSON

		// TODO Асинхронная передача на сервер

	}

	virtual bool connectToCommCtl()
	{
		return true;
	}

};

#endif /* HTTPCLIENT_H_ */
