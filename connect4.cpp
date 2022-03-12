#define PROLOG_MODULE "user"
#include "SWI-cpp.h"
#include <cstdio>
#include <stdio.h>
#include <iostream>
#include <memory>
#include <stdexcept>
#include <string>
#include <array>
#include <sstream>
#include <map>

/*
@param: current state of the game
@return: next move of "perfect player"
*/

PREDICATE(readColumn2, 2)
{ 
	//Handle current state of the game
	PlTail tail(A2);
 	PlTerm e;
	std::string state = "";
	std::map<int,int> counter;
	
	for (int i = 1; i <= 6; i++) 
	{
		counter[i] = 0;
	}
	
	while(tail.next(e)) 
	{
  		int i = (atoi((char *)e) + 1);
  		counter[i] = counter[i] + 1;
   		state +=  std::to_string(i);
    	}
    	
	//Prepare and send request to find the best move following
    	std::string cmd = "curl --location --request GET 'https://connect4.gamesolver.org/solve?pos=" + state + "'";
    	std::array<char, 128> buffer;
    	std::string result;
    	std::unique_ptr<FILE, decltype(&pclose)> pipe(popen(cmd.c_str(), "r"), pclose);
    	if (!pipe) {
        	throw std::runtime_error("popen() failed!");
    	}
    	while (fgets(buffer.data(), buffer.size(), pipe.get()) != nullptr) {
        	result += buffer.data();
    	}
    
    	//Handle response
    	std::size_t pos1 = result.find('[');
    	std::size_t pos2 = result.find(']');
    	std::string string = result.substr(pos1 + 1, pos2 - 1 - pos1);
    	string = "x," + string;
    
    
    	char *cstring = new char[string.length() + 1];
    	strcpy(cstring, string.c_str());
    
    
    	char * token = strtok (cstring,",");
     
    	int i = 0;
    	int last = 0;
    	int pos = 0;
    	while (token != NULL)
   	{
      		token = strtok (NULL, ",");
      		if (token != NULL) 
      		{
        		int val = atoi(token);
        		if (i == 0) 
        		{
        			last = val;
       		} 
       		else if (val > last && counter[i + 1] < 6) 
       		{
          			last = val;
          			pos = i;
        		}
      		}
      
      		i++;
    	}
    
    	return A1 = pos ;
}

