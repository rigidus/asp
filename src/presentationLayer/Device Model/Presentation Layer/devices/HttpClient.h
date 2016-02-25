/*
 * clientHttp.h
 *
 *  Created on: 24 февр. 2016 г.
 *      Author: alex
 */

#ifndef HTTPCLIENT_H_
#define HTTPCLIENT_H_

#include <mongoose/mongoose.h>

class HttpClient: public CBaseDevice
{

// CodecType protoCodec;
	  struct mg_mgr mgr;

public:

	HttpClient(): CBaseDevice(s_concreteName)
	{
		  mg_mgr_init(&mgr, NULL);
	}

	~HttpClient()
	{
	}

	static const std::string s_concreteName;

	std::vector<uint8_t> rcvData;

	static volatile int s_exit_flag;

	static void ev_handler(struct mg_connection *nc, int ev, void *ev_data) {
	  struct http_message *hm = (struct http_message *) ev_data;

	  switch (ev) {
	    case MG_EV_CONNECT:
	      if (* (int *) ev_data != 0) {
	        fprintf(stderr, "connect() failed: %s\n", strerror(* (int *) ev_data));
	        s_exit_flag = 1;
	      }
	      break;
	    case MG_EV_HTTP_REPLY:
	      nc->flags |= MG_F_CLOSE_IMMEDIATELY;
//	      if (s_show_headers) {
//	        fwrite(hm->message.p, 1, hm->message.len, stdout);
//	      } else {
//	        fwrite(hm->body.p, 1, hm->body.len, stdout);
//	      }
	      putchar('\n');
	      s_exit_flag = 1;
	      break;
	    default:
	      break;
	  }
	}

	virtual void sendCommand(const std::string command, const std::string pars)
	{
		std::cout << "Businness logic client performs command: " << command << "[" << pars << "]" << std::endl;

		// TODO Сериализация JSON

		// TODO Здесь пока что сделана синхронка for examle, но нужна Асинхронная передача на сервер
		// Для чего надо ставить задачу на отправку тредпулу, а подтверждение обрабатывать в коллбэке
		s_exit_flag = 0;
		mg_connect_http(&mgr, ev_handler, "http://127.0.0.1:8000", NULL, (command+pars).c_str());

		while (s_exit_flag == 0) {
			mg_mgr_poll(&mgr, 1000);
		}
	}

	virtual bool connectToCommCtl()
	{
		return true;
	}

};

#endif /* HTTPCLIENT_H_ */
