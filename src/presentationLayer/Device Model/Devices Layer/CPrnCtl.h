#if !defined(_CPRNCTL_H_)
#define _CPRNCTL_H_

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

/*
 * Multitone class controls LCD interface for interacting with concrete device
 */
class CPrnCtl : private CBaseCommCtl
{

	struct TPrinterData{
		std::string name;
		std::string filename;
		int32_t fd;
	};

public:
	/*
	 * Function takes GPIO resourse for concrete device
	 * @param device - pointer to concrete device use for call callback for inform about event from device
	 * @param gpioName - name of the GPIO resourse for taking
	 * @return - pointer to busied resource or nullptr when resourse is busy or not existing
	 */
	static shared_ptr<CBaseCommCtl> takeCommCtl(CBaseDevice* device, const std::string& printerName);

	/*
	 * Function frees GPIO resource for it can be taking by another device in the future
	 * @param device - pointer to concrete device for control. Device-taker can free GPIO resourse only
	 * @param gpioName - name of the GPIO resourse for free
	 */
	static void freeCommCtl(CBaseDevice* device, const std::string& printerName);

	virtual ~CPrnCtl();

	static const std::string s_name;

	virtual uint32_t send(std::list<std::vector<uint8_t> > sendData);
	virtual uint32_t send(std::vector<uint8_t> sendData);

private:
	CPrnCtl(CBaseDevice* device, const settings::CommPrinterConfig& config, TPrinterData& printerData);

	const settings::CommPrinterConfig m_Config;
	TPrinterData m_PrinterData;

	// check existing file on the filesystem
	// It use here for check interface /dev/usb/lp0 file only
	static bool fileIsExist(const std::string& fileName);
	static settings::CommPrinterConfig getPrinterConfig(CBaseDevice* device, const std::string& printerName);
	static std::map<std::string, shared_ptr<CBaseCommCtl> > busyPrinters;

	static const std::string printerPath;
//	static bool stopFlag;

	bool checkFiles();
	void setupPrinter();

	std::filebuf fBuf;
	std::fstream fLog;

	uint32_t m_timeout;


};



#endif //_CPRNCTL_H_
