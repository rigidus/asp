/*
 * MyThreadPool.h
 *
 *  Created on: 9 февр. 2016 г.
 *      Author: alex
 */

#ifndef MYTHREADPOOL_H_
#define MYTHREADPOOL_H_

#define DEBUG

#include <iostream>
#include <queue>
#include <list>
#include <sstream>
#include <map>
#include <functional>
#include <boost/detail/atomic_count.hpp>
#include <boost/thread/thread.hpp>
#include <boost/thread/mutex.hpp>
#include <boost/smart_ptr.hpp>
#include <boost/bind.hpp>
#include <boost/function.hpp>
#include <boost/date_time/posix_time/posix_time.hpp>
#include <boost/thread/thread_time.hpp>

#include "../include/FileLog.h"

typedef unsigned int	u32;
typedef unsigned char	u08;

namespace mythreadpool {

class CThreadPool;
class CWorker;

typedef boost::function<void(void)>		TTaskFunc;

struct PriorityTask
{
	u32 p;
	TTaskFunc fn;

	explicit PriorityTask(u32 _p, TTaskFunc _fn): p(_p), fn(_fn) {}

};

class CTaskQueue
{

	friend CThreadPool;
	friend CWorker;

	mutable boost::mutex mut;
	boost::condition_variable cond;

	std::map<u32, std::queue<TTaskFunc>, std::greater<u32> > qTasks;

	void push(PriorityTask& t)
	{
		boost::mutex::scoped_lock lock(mut);

		qTasks[t.p].push(t.fn);
	}

	size_t size() const
	{
		boost::mutex::scoped_lock lock(mut);

		return qTasks.size();
	}

	bool GetTask(TTaskFunc& t)
	{
		boost::mutex::scoped_lock lock(mut);
		if (qTasks.size())
		{
			std::map<u32, std::queue<TTaskFunc>, std::greater<u32> >::iterator it = qTasks.begin();

			t = it->second.front();
			it->second.pop();
			CFileLog::cfilelog() << "Get task with priority = " << it->first << std::endl;

			if (!it->second.size()) qTasks.erase(it);

			return true;
		} else {

			return false;
		}
	}

};

class CWorker
{
private:

	CWorker():
		IsRunning(false),
		IsEnd(false),
		IsFinished(false),
		taskqueue(nullptr),
		thr(nullptr),
		id(0)
	{}

	friend CThreadPool;

	volatile bool IsRunning;
	volatile bool IsEnd;
	volatile bool IsFinished;

	CTaskQueue* taskqueue;

	boost::mutex mut;
	boost::thread *thr;

	uint32_t id;

	static void WorkerFn(CWorker* mystate)
	{

		CFileLog::cfilelog() << "Work thread " << mystate->id <<  ": Start" << std::endl;
		TTaskFunc fn;

		try
		{
			mystate->IsRunning = true;
			while (mystate->IsRunning)
			{

				boost::this_thread::interruption_point();

				if (mystate->taskqueue->GetTask(fn))
				{

					CFileLog::cfilelog() << "Work thread " << mystate->id << ": Task running" << std::endl;

					boost::this_thread::disable_interruption d;
					fn();
					CFileLog::cfilelog() << "Work thread " << mystate->id << ": Task ready" << std::endl;

					if (mystate->IsEnd) break;

				}else{


					CFileLog::cfilelog() << "Work thread " << mystate->id << ": Task waiting..." << std::endl;

					boost::mutex::scoped_lock lock(mystate->mut);
					if (mystate->IsEnd) break;
					mystate->taskqueue->cond.wait(lock);

					CFileLog::cfilelog() << "Work thread " << mystate->id << ": Try get new task" << std::endl;
				}
			}

			mystate->IsRunning = false;
			mystate->IsEnd = true;
		}

		catch(boost::thread_interrupted& e)
		{
			CFileLog::cfilelog() << "Work thread " << mystate->id << ":  End with exception thread_interrupted" << std::endl;
			mystate->IsFinished = true;
			return;
		}

		catch(std::exception& ex)
		{
			CFileLog::cfilelog() << "Work thread " << mystate->id << ":  End with std::exception: " << ex.what() << std::endl;
			mystate->IsFinished = true;
			return;
		}

		catch(...)
		{
			CFileLog::cfilelog() << "Work thread " << mystate->id << ":  End with unknown exception" << std::endl;
			mystate->IsFinished = true;
			return;
		}

		CFileLog::cfilelog() << "Work thread " << mystate->id << ":  End" << std::endl;
		mystate->IsFinished = true;

	}

public:

	~CWorker()
	{
		CFileLog::cfilelog() << "Destructor CWorker: " << id  << std::endl;

		taskqueue->cond.notify_all();

		if ( !IsFinished )
		{
			thr->interrupt();
		}

		taskqueue->cond.notify_all();

		if (thr->joinable()) thr->join();
		thr->detach();

		CFileLog::cfilelog() << "Delete Thread: " << id  << std::endl;
		delete thr;
		CFileLog::cfilelog() << "Thread deleted: " << id  << std::endl;
	}

	CWorker(CTaskQueue* tq, u32 n):
		IsRunning(false),
		IsEnd(false),
		IsFinished(false),
		taskqueue(tq),
		thr(nullptr),
		id(n)
	{

		CFileLog::cfilelog() << "Create Thread: " << id << std::endl;

		thr = new boost::thread(WorkerFn, this);

	}

	u32 get_id() const
	{
		return id;
	}

};


class CThreadPool
{

	CThreadPool(CThreadPool& obj):
		maxtask(100),
		maxthreads(2)
	{}

	CThreadPool operator =(const CThreadPool obj) { return *this; }

	boost::mutex mutChangeThreads;
	boost::mutex mutAddTask;


protected:

	CTaskQueue qTasks;

	u32 maxtask;
	u32 maxthreads;

	std::list<CWorker*> vWorks;

	bool StartThreads(u32 n)
	{
		try
		{
			std::unique_ptr<CWorker> vec[n];

			u32 cnt = vWorks.size() ? vWorks.back()->id : 0;

			for (u32 i=0; i<n; ++i, cnt++)
			{
				std::unique_ptr<CWorker> w(new CWorker(&qTasks, cnt));
				std::swap(w, vec[i]);
			}

			for (u32 i=0; i<n; ++i)
			{
				vWorks.push_back(vec[i].release());
			}

		}

		catch(...)
		{
			CFileLog::cfilelog() << "StartThreads Error!" << std::endl;

			return false;
		}

		return true;
	}

	bool StopThreads(u32 n)
	{

		std::list<CWorker*>::iterator itend = vWorks.begin();
		while (n)
		{
			(*itend)->IsRunning = false;
			(*itend)->IsEnd = true;
			++itend; --n;
		}

		DeleteThreads(vWorks.begin(), itend);

		return true;
	}

	void DeleteThreads(std::list<CWorker*>::iterator itbgn, std::list<CWorker*>::iterator itend)
	{
		for (std::list<CWorker*>::iterator it = itbgn; it != itend; ++it)
		{
			delete *it;
		}

		vWorks.erase(itbgn, itend);
	}

	void InternalForceStop()
	{

		for (std::list<CWorker*>::iterator it=vWorks.begin(); it != vWorks.end(); ++it)
		{
			(*it)->IsRunning = false;
			(*it)->IsEnd = true;
		}

		DeleteThreads(vWorks.begin(), vWorks.end());
	}

public:

	CThreadPool(u32 thrn, u32 _maxtask, u32 _maxthreads=100): maxtask(_maxtask), maxthreads(_maxthreads)
	{

		CFileLog::cfilelog() << "Enter to ThreadPool constructor" << std::endl;

		SetThreads(thrn);

	}

	~CThreadPool()
	{
		boost::mutex::scoped_lock lock(mutChangeThreads);

		CFileLog::cfilelog() << "Enter to ThreadPool destructor" << std::endl;

		InternalForceStop();
	}

	bool SetThreads(u32 thrn)
	{
		boost::mutex::scoped_lock lock(mutChangeThreads);

		if (thrn > maxthreads)
		{
			CFileLog::cfilelog() << "Warning: Quantity of work threads more than enabled maximum. Let's set value less or equal than " << maxthreads << std::endl;
			return false;
		}

		CFileLog::cfilelog() << "Set Threads(" << thrn  << ")" << std::endl;

		if (thrn < vWorks.size() )
			return StopThreads(thrn);

		else
			return StartThreads(thrn-vWorks.size());
	}

	bool AddTask(u32 priority, TTaskFunc func)
	{

		boost::mutex::scoped_lock lock(mutAddTask);

		if (qTasks.size() >= maxtask )
		{
			CFileLog::cfilelog() << "Warning: Task adding failed because maximum tasks have inserted already" << std::endl;
			return false;
		}

		if (!vWorks.size())
			CFileLog::cfilelog() << "Warning: Quantity of work threads = 0" << std::endl;

		CFileLog::cfilelog() << "Add Task with priority=" << priority << std::endl;

		PriorityTask p(priority, func);

		qTasks.push(p);
		qTasks.cond.notify_one();

		return true;
	}

	void ForceStop()
	{
		boost::mutex::scoped_lock lock(mutChangeThreads);

		CFileLog::cfilelog() << "Force Stop" << std::endl;

		InternalForceStop();
	}

	void DoTasksAndStop()
	{

		boost::mutex::scoped_lock lock(mutChangeThreads);

		CFileLog::cfilelog() << "Do Tasks And Stop" << std::endl;

		for (std::list<CWorker*>::iterator it=vWorks.begin(); it != vWorks.end(); ++it)
		{
			(*it)->IsEnd = true;
		}

		DeleteThreads(vWorks.begin(), vWorks.end());
	}

	void SetMaxTask(u32 n)
	{
		boost::mutex::scoped_lock lock(mutAddTask);

		CFileLog::cfilelog() << "Set MaxTasks(" << n  << ")" << std::endl;

		maxtask = n;
	}

};

} // namespace mythreadpool

#endif /* MYTHREADPOOL_H_ */
