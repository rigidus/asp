################################################################################
# Automatically-generated file. Do not edit!
################################################################################

# Add inputs and outputs from these tool invocations to the build variables 
CPP_SRCS += \
../CBaseCodec.cpp \
../CBaseCommCtl.cpp \
../CDeviceManager.cpp \
../CDisplayCtl.cpp \
../CPinCodec.cpp \
../CPinCtl.cpp \
../CPrnCtl.cpp \
../CSerialPortCtl.cpp \
../DeviceLayerSettingsDemo.cpp \
../Logger.cpp \
../NameInstanses.cpp \
../SetCommandTo.cpp \
../Settings.cpp \
../presentationMain.cpp 

OBJS += \
./CBaseCodec.o \
./CBaseCommCtl.o \
./CDeviceManager.o \
./CDisplayCtl.o \
./CPinCodec.o \
./CPinCtl.o \
./CPrnCtl.o \
./CSerialPortCtl.o \
./DeviceLayerSettingsDemo.o \
./Logger.o \
./NameInstanses.o \
./SetCommandTo.o \
./Settings.o \
./presentationMain.o 

CPP_DEPS += \
./CBaseCodec.d \
./CBaseCommCtl.d \
./CDeviceManager.d \
./CDisplayCtl.d \
./CPinCodec.d \
./CPinCtl.d \
./CPrnCtl.d \
./CSerialPortCtl.d \
./DeviceLayerSettingsDemo.d \
./Logger.d \
./NameInstanses.d \
./SetCommandTo.d \
./Settings.d \
./presentationMain.d 


# Each subdirectory must supply rules for building sources it contributes
%.o: ../%.cpp
	@echo 'Building file: $<'
	@echo 'Invoking: Cross G++ Compiler'
	arm-linux-gnueabihf-g++ -std=c++11 -DARM_STATIC_BUILD -I"/home/alex/workspace/asp/src/presentationLayer/Device Model/Devices Layer" -I/usr/local/arm-linux-gnueabihf/ -I/usr/local/arm-linux-gnueabihf/boost -I/usr/local/arm-linux-gnueabihf/arm-linux-gnueabihf/libc/usr/include -I/usr/local/arm-linux-gnueabihf/arm-linux-gnueabihf/include/c++/4.9.2 -I/usr/local/arm-linux-gnueabihf/arm-linux-gnueabihf/include/c++/4.9.2/arm-linux-gnueabihf -O0 -g3 -Wall -c -fmessage-length=0 -MMD -MP -MF"$(@:%.o=%.d)" -MT"$(@)" -o "$@" "$<"
	@echo 'Finished building: $<'
	@echo ' '


