///////////////////////////////////////////////////////////
//  CTcpSocketFactory.cpp
//  Implementation of the Class CTcpSocketFactory
//  Created on:      20-���-2016 16:20:20
//  Original author: user-PC
///////////////////////////////////////////////////////////

#include "CTcpSocketFactory.h"


CTcpSocketFactory::CTcpSocketFactory(){

}


CTcpSocketFactory::CTcpSocketFactory(const CTcpSocketFactory& theCTcpSocketFactory){

}


CTcpSocketFactory::CTcpSocketFactory(CTcpSocketFactory& rhs){

}


CTcpSocketFactory& CTcpSocketFactory::operator=(CTcpSocketFactory& rhs){

	return  *this;
}


CTcpSocketFactory* CTcpSocketFactory::getSocketFactory(){

	return  NULL;
}
