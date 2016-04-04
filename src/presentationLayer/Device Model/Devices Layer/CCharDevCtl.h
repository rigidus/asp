#if !defined(_CSCNUSBCTL_H_)
#define _CSCNUSBCTL_H_

#include "CBaseCommCtl.h"
#include "devices/CBaseDevice.h"
#include "GlobalThreadPool.h"

#include <iostream>
#include <map>
#include <boost/asio.hpp>
#include <boost/thread.hpp>
#include <boost/filesystem.hpp>
#include <boost/iostreams/device/file.hpp>
#include <boost/iostreams/stream.hpp>

namespace io = boost::iostreams;

class CCharDevCtl : private CBaseCommCtl
{

	struct TCharDevData{
		std::string name;
		std::string filename;
		int32_t fd;
		int32_t events;
		int32_t watch;
		int32_t oldvalue;
	};

public:

	/*
	 * Function takes character device of /dev/... for concrete device
	 * @param device - pointer to concrete device use for call callback for inform about event from device
	 * @param deviceName - name of the character device resourse for taking
	 * @return - pointer to busied resource or nullptr when resourse is busy or not existing
	 */
	static shared_ptr<CBaseCommCtl> takeCommCtl(CBaseDevice* device, const std::string& deviceName);

	/*
	 * Function frees character device resource for it can be taking by another device in the future
	 * @param device - pointer to concrete device for control. Device-taker can free GPIO resourse only
	 * @param deviceName - name of the GPIO resourse for free
	 */
	static void freeCommCtl(CBaseDevice* device, const std::string& deviceName);

	virtual ~CCharDevCtl();

	static const std::string s_name;

	static boost::thread* thrNotify;

	// Thread function for waiting character device events
	// тред должен ждать события на символьном устройстве через интерфейс inotify
	// тред должен как-то управляться из того же места, где
	// будут управляться все треды комм. девайсов
	static void Notifier();

	static void startNotifier();
	static void stopNotifier();

	// CPinCtl public members
	virtual uint32_t send(std::list<std::vector<uint8_t> > sendData);
	virtual uint32_t send(std::vector<uint8_t> sendData);
	int8_t getCharDevValue(std::string &value);

private:
	CCharDevCtl(CBaseDevice* device, const settings::CommCharDevConfig& config, TCharDevData& charData);

	const settings::CommCharDevConfig m_Config;
	TCharDevData m_CharDevData;

	// check existing file on the filesystem
	// It use here for check device interface files only
	static bool fileIsExist(const std::string& fileName);
	static settings::CommCharDevConfig getCharDevConfig(const std::string& deviceName);
	static std::map<std::string, shared_ptr<CBaseCommCtl> > busyCharDevs;

	static const std::string devicePath;
	static bool stopFlag;

	bool checkFiles();
	void setupDevice();

	std::filebuf fBuf;
	std::fstream fLog;

	uint32_t m_timeout;


};

#endif // _CSCNUSBCTL_H_
