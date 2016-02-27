/*
 * clientHttp.h
 *
 *  Created on: 24 февр. 2016 г.
 *      Author: alex
 */

#ifndef HTTPCLIENT_H_
#define HTTPCLIENT_H_

#include <boost/thread/mutex.hpp>
#include <mongoose/mongoose.h>
#include "SetCommandTo.h"

class HttpClient: public CBaseDevice
{
	struct mg_mgr mgr;

public:

	HttpClient(): CBaseDevice(s_concreteName)
	{
		mg_mgr_init(&mgr, NULL);
	}

	~HttpClient()
	{
		mg_mgr_free(&mgr);
	}

	static const std::string s_concreteName;

	std::vector<uint8_t> rcvData;

	static volatile int s_exit_flag;

	static boost::mutex HTTPconnectMutex;

	static void ev_handler(struct mg_connection *nc, int ev, void *ev_data) {
	  struct http_message *hm = (struct http_message *) ev_data;

		switch (ev) {

		case MG_EV_CONNECT:
			if (* (int *) ev_data != 0) {
				std::cout << "HttpClient::ev_handler: Connect to businness logic failed: " << strerror(* (int *) ev_data);
				s_exit_flag = 1;
			}
			break;

		case MG_EV_HTTP_REPLY:
			{
				nc->flags |= MG_F_CLOSE_IMMEDIATELY;
				std::cout << "HttpClient::ev_handler: message transferred, response code: " << hm->resp_code << std::endl;
				s_exit_flag = 1;

				setCommandTo::Manager(HttpClient::s_concreteName);
			}
			break;

		default:
			break;

	  }
	}

	virtual void sendCommand(const std::string command, const std::string pars)
	{

		boost::mutex::scoped_lock lock(HTTPconnectMutex);

		std::cout << "HttpClient::sendCommand: Business logic client performs command: " << command << "[" << pars << "]" << std::endl;

		// TODO Сериализация JSON

		// TODO Здесь пока что сделана синхронка for examle, но нужна Асинхронная передача на сервер
		// Для чего надо ставить задачу на отправку тредпулу, а подтверждение обрабатывать в коллбэке
		// и ставить задачу для дев менеджера для подтверждения транзакции, удаления ее из очереди и старта
		// следующей.
		s_exit_flag = 0;

		mg_connect_http(&mgr, ev_handler, "http://127.0.0.1:8000", "", (command+pars).c_str());

		while (s_exit_flag == 0) {
			mg_mgr_poll(&mgr, 1000);
		}

		std::cout << "HttpClient::sendCommand: Business logic client end command: " << command << "[" << pars << "]" << std::endl;

	}

	virtual bool connectToCommCtl()
	{
		return true;
	}

};

#endif /* HTTPCLIENT_H_ */
