/*
 * multicurlAdapter.h
 *
 *  Created on: 17 февр. 2016 г.
 *      Author: alex
 */

#ifndef CURLADAPTER_H_
#define CURLADAPTER_H_

#include "curl/curl.h"
#include <string>
#include <iostream>

#include <boost/thread.hpp>

class CurlAdapter
{

	CURLM* multicurl;
	int handle_count;

	char errorBuffer[CURL_ERROR_SIZE];

	static std::string buffer;

	static uint32_t writer(char *data, size_t size, size_t nmemb, std::string* buffer)
	{
		uint32_t result = 0;

		if (buffer != nullptr)
		{
			buffer->append(data, size * nmemb);
			result = size * nmemb;
		}

		if (result == CURLE_OK)
		{
			s_cbFunction(*buffer);
			buffer->clear();
		}

		return result;
	}

	void CurlSetup(CURL* curl)
	{
		if (curl)
		{
			curl_easy_setopt(curl, CURLOPT_ERRORBUFFER, errorBuffer);
//			curl_easy_setopt(curl, CURLOPT_URL, "http://localhost");
			curl_easy_setopt(curl, CURLOPT_URL, "http://www.google.ru");
			curl_easy_setopt(curl, CURLOPT_WRITEFUNCTION, CurlAdapter::writer);
			curl_easy_setopt(curl, CURLOPT_WRITEDATA, &CurlAdapter::buffer);
		}
	}

public:

	typedef void(*TFnCurlCallback)(std::string);
	static TFnCurlCallback s_cbFunction;

	CurlAdapter(): multicurl(nullptr), handle_count(0)
	{	}

	~CurlAdapter()
	{
		if (multicurl)
			curl_multi_cleanup(multicurl);
	}

	bool CurlStart( TFnCurlCallback cbFunction )
	{
		multicurl = curl_multi_init();

		if (multicurl)
		{
			s_cbFunction = cbFunction;
			return true;
		}

		return false;
	}

	bool AddPost(const char* string)
	{
		if (!multicurl) return false;

		return true;
	}

	bool AddRequest(const char* string)
	{
		if (!multicurl) return false;

		CURL* curl = NULL;
		curl = curl_easy_init();

		if (!curl) return false;

		CurlSetup(curl);
		curl_multi_add_handle(multicurl, curl);

		return true;
	}

	void Update()
	{
		if (!multicurl) return;

		std::cout << curl_multi_perform(multicurl, &handle_count) << std::endl;
	}

};

CurlAdapter::TFnCurlCallback CurlAdapter::s_cbFunction = nullptr;
std::string CurlAdapter::buffer;

#endif /* CURLADAPTER_H_ */
