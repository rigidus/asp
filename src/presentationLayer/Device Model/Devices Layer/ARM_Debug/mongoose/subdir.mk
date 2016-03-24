################################################################################
# Automatically-generated file. Do not edit!
################################################################################

# Add inputs and outputs from these tool invocations to the build variables 
C_SRCS += \
../mongoose/mongoose.c 

OBJS += \
./mongoose/mongoose.o 

C_DEPS += \
./mongoose/mongoose.d 


# Each subdirectory must supply rules for building sources it contributes
mongoose/%.o: ../mongoose/%.c
	@echo 'Building file: $<'
	@echo 'Invoking: Cross GCC Compiler'
	arm-linux-gnueabihf-gcc -std=c11 -I/usr/local/arm-linux-gnueabihf/ -I/usr/local/arm-linux-gnueabihf/boost -I/usr/local/arm-linux-gnueabihf/arm-linux-gnueabihf/libc/usr/include -I/usr/local/arm-linux-gnueabihf/arm-linux-gnueabihf/include/c++/4.9.2 -I/usr/local/arm-linux-gnueabihf/arm-linux-gnueabihf/include/c++/4.9.2/arm-linux-gnueabihf -O0 -g3 -Wall -c -fmessage-length=0 -MMD -MP -MF"$(@:%.o=%.d)" -MT"$(@)" -o "$@" "$<"
	@echo 'Finished building: $<'
	@echo ' '


