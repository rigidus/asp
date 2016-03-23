#if !defined(_CDISPLAYCTL_H_)
#define _CDISPLAYCTL_H_

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

#define IOCTL_CLEAR_DISPLAY 	  	'0'   // Identifiers for ioctl reqursts
#define IOCTL_PRINT_ON_FIRSTLINE  	'1'

/*
 * Multitone class controls LCD interface for interacting with concrete device
 */
class CDisplayCtl : private CBaseCommCtl
{

	struct TDisplayData{
		std::string name;
		std::string filename;
		int32_t fd;
//		int32_t events;
//		int32_t watch;
//		int32_t oldvalue;
	};

public:
	/*
	 * Function takes GPIO resourse for concrete device
	 * @param device - pointer to concrete device use for call callback for inform about event from device
	 * @param gpioName - name of the GPIO resourse for taking
	 * @return - pointer to busied resource or nullptr when resourse is busy or not existing
	 */
	static shared_ptr<CBaseCommCtl> takeCommCtl(CBaseDevice* device, const std::string& displayName);

	/*
	 * Function frees GPIO resource for it can be taking by another device in the future
	 * @param device - pointer to concrete device for control. Device-taker can free GPIO resourse only
	 * @param gpioName - name of the GPIO resourse for free
	 */
	static void freeCommCtl(CBaseDevice* device, const std::string& displayName);

	virtual ~CDisplayCtl();

	static const std::string s_name;

//	static boost::thread* thrNotify;

	// Thread function for waiting GPIO events
	// тред должен ждать события на пине через интерфейс inotify
	// тред должен как-то управляться из того же места, где
	// будут управляться все треды комм. девайсов
//	static void Notifier();

//	static void startNotifier();
//	static void stopNotifier();


	// CDisplayCtl public members
	virtual uint32_t send(std::list<std::vector<uint8_t> > sendData);
	virtual uint32_t send(std::vector<uint8_t> sendData);

private:
	CDisplayCtl(CBaseDevice* device, const settings::CommDisplayConfig& config, TDisplayData& displayData);

	const settings::CommDisplayConfig m_Config;
	TDisplayData m_DisplayData;

	// check existing file on the filesystem
	// It use here for check interface /dev/klcd file only
	static bool fileIsExist(const std::string& fileName);
	static settings::CommDisplayConfig getDisplayConfig(CBaseDevice* device, const std::string& displayName);
	static std::map<std::string, shared_ptr<CBaseCommCtl> > busyDisplays;

	static const std::string displayPath;
//	static bool stopFlag;

	bool checkFiles();
	void setupDisplay();

	std::filebuf fBuf;
	std::fstream fLog;

	uint32_t m_timeout;


};




#endif //_CDISPLAYCTL_H_
