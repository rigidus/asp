################################################################################
# Automatically-generated file. Do not edit!
################################################################################

# Add inputs and outputs from these tool invocations to the build variables 
CPP_SRCS += \
../devices/CBaseDevice.cpp 

OBJS += \
./devices/CBaseDevice.o 

CPP_DEPS += \
./devices/CBaseDevice.d 


# Each subdirectory must supply rules for building sources it contributes
devices/%.o: ../devices/%.cpp
	@echo 'Building file: $<'
	@echo 'Invoking: Cross G++ Compiler'
	arm-linux-gnueabihf-g++ -std=c++11 -DARM_STATIC_BUILD -I"/home/alex/workspace/asp/src/presentationLayer/Device Model/Devices Layer" -I/usr/local/arm-linux-gnueabihf/ -I/usr/local/arm-linux-gnueabihf/boost -I/usr/local/arm-linux-gnueabihf/arm-linux-gnueabihf/libc/usr/include -I/usr/local/arm-linux-gnueabihf/arm-linux-gnueabihf/include/c++/4.9.2 -I/usr/local/arm-linux-gnueabihf/arm-linux-gnueabihf/include/c++/4.9.2/arm-linux-gnueabihf -O0 -g3 -Wall -c -fmessage-length=0 -MMD -MP -MF"$(@:%.o=%.d)" -MT"$(@)" -o "$@" "$<"
	@echo 'Finished building: $<'
	@echo ' '


