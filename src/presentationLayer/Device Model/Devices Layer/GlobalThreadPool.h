//============================================================================
// Name        : GlobalThreadPool.h
// Author      : aav
// Created on  : 11 февр. 2016 г.
// Version     : v.0.2
// Copyright   : Non nobis, Domine, non nobis, sed nomini tuo da gloriam.
// Description : Singleton for MyThreadPool
//============================================================================

#ifndef GLOBALTHREADPOOL_H_
#define GLOBALTHREADPOOL_H_

#include "MyThreadPool.h"

class GlobalThreadPool
{

	static boost::mutex mut;

	static mythreadpool::CThreadPool& getThreadPool(bool stop)
	{
		static mythreadpool::CThreadPool* threadPool = nullptr;

		boost::mutex::scoped_lock(mut);

		if (stop)
		{
			if (threadPool != nullptr)
			{
				delete threadPool;
				threadPool = nullptr;
			}
		}
		else
		{
			if (threadPool == nullptr)
			{
				threadPool = new mythreadpool::CThreadPool(4, 100, 100);
			}
		}

		return *threadPool;
	}

public:

	static mythreadpool::CThreadPool& get()
	{
		return GlobalThreadPool::getThreadPool(false);
	}

	static void stop()
	{
		GlobalThreadPool::getThreadPool(true);
	}

};



#endif /* GLOBALTHREADPOOL_H_ */
