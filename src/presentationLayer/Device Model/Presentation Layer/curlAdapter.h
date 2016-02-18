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

//#ifdef __cplusplus
//extern "C"
//{
//#endif

class CurlAdapter
{

	CURLM* multicurl;
	int handle_count;

	char errorBuffer[CURL_ERROR_SIZE];

	typedef void(*TFnCurlCallback)(std::string);
	typedef int (*TFnCbWrite)(char*, size_t, size_t, std::string*);

	TFnCurlCallback m_cbFunction;

	TFnCbWrite cbFn;

	std::string m_buffer;

	static int writer(char *data, size_t size, size_t nmemb, std::string* buffer)
	{

		std::cout << "CurlAdapter::writer was called " << std::endl;

		uint32_t result = 0;

		if (buffer != nullptr)
		{
			std::cout << "CurlAdapter::writer before append: " << size << ", " << nmemb << std::endl;
			buffer->append(data, size * nmemb);
			std::cout << "CurlAdapter::writer after append" << std::endl;
			result = size * nmemb;
		}

		std::cout << "CurlAdapter::writer was ended" << std::endl;

		return result;
	}

	void CurlSetup(CURL* curl)
	{
		if (curl)
		{
			curl_easy_setopt(curl, CURLOPT_ERRORBUFFER, errorBuffer);
//			curl_easy_setopt(curl, CURLOPT_URL, "http://localhost");
			curl_easy_setopt(curl, CURLOPT_URL, "http://www.google.ru");
			curl_easy_setopt(curl, CURLOPT_WRITEFUNCTION, CurlAdapter::cbFn);
			curl_easy_setopt(curl, CURLOPT_WRITEDATA, &m_buffer);
		}
	}

public:

	std::vector<CURL*> vCurls;

	CurlAdapter(): multicurl(nullptr), handle_count(0)
	{
		cbFn = &CurlAdapter::writer;
		m_cbFunction = nullptr;
	}

	~CurlAdapter()
	{

		for (CURL* curl: vCurls)
			curl_easy_cleanup(curl);

		if (multicurl)
			curl_multi_cleanup(multicurl);
	}

	bool CurlStart( TFnCurlCallback cbFunction )
	{
		multicurl = curl_multi_init();

		if (multicurl)
		{
			m_cbFunction = cbFunction;
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

		{
			CURL* curl = NULL;
			curl = curl_easy_init();

			if (!curl) return false;

			vCurls.push_back(curl);

			CurlSetup(curl);
			curl_multi_add_handle(multicurl, curl);
		}

		return true;
	}

	int32_t Update()
	{
		if (!multicurl) return CURLM_OK-2;

		CURLMcode result = CURLM_CALL_MULTI_PERFORM;
		while(1)
		{
			result = curl_multi_perform(multicurl, &handle_count);
			if (handle_count == 0)
			{
				if (result == CURLM_OK)
				{
					m_cbFunction(m_buffer);
					m_buffer.clear();
				}

				break;
			}
		}

		return result;
	}

};

//CurlAdapter::TFnCurlCallback CurlAdapter::s_cbFunction = nullptr;
//std::string CurlAdapter::buffer;

//#ifdef __cplusplus
//}
//#endif

#endif /* CURLADAPTER_H_ */
