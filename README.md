# Echo bot for Telegram and VK 

<p> Telegram bot works properly and supports any types of messages (with or without attachments).  </p>
<p> VK bot can process text messages, /repeat and /help commands, map coordinates, links, stickers, audio, video attachments. If there is an attachment the VK bot cannot process, the attachment will be ignored.   </p>

## Project structure description

<p> The project is devided into three parts: app (Main module), src (see the structure of this part below), test (contains unit tests). </p> 

### Src structure

<p> src part contains the following folders: </p>

- API  
- Config  
- Environment 
- Exceptions  
- Logic  
- Services  
- TextMessages

#### API 
<p> API folder contains modules Bot (runBot and nextLoop functions) and Wrapper (exports getPureMessageList and sendM functions) </p>

<p> The <b>Telegram</b> folder contains module Cleaners (exports telByteStringToPureMessageList function and contains functions for decoding json data Telegram server sends into list of PureMessages), there are some functions for decoding in Cleaners folder. Module Data contains some specific data for connecting to Telegram server and some functions for building Telegram http requests. You can find specific data structures and FromJSON/ToJSON instances of Telegram server responses in Structs folder. </p>

<p> The <b>VK</b> folder contains module Data (specific data for connecting to VK server and some functions for building VK http requests). The structure of VK message is more complicated than Telegram's one so you can find functions for decoding VK response bytestrings into PureMessages in separate folder Cleaners: module ToPureMsgList exports vkByteStringToPureMessageList function and uses functions from other modules and folders here to decode VK server responses into list of PureMessages. Structs folder contains specific data structures and FromJSON/ToJSON instances of VK server responses.</p>

<p> The <b>Wrapper</b> folder contains the following modules: Funtions module (functions for building http requests with UrlEncoded body or Multipart body); GetResponseFunctions module (contains getResponseMultipart and getResponseUrl functions); GetUpdate module (contains getPureMessageList function: it gets updates from server and decodes them into list of PureMEssages); SendMessage module (contains the sendM function: it sends the PureMessage back to user and changes the offset); Structs module (contains data structures for building http reuests). </p>

#### Config
<p> Config folder contains the following modules: </p>

- Data module - some information to connect to VK long poll server (url, API version and Time out for both VK and Telegram)
- Functions module - contains configSetOffset function and configGetUid (get update ID) functions
- Structs module - contains Config data type definition 
- Exports module - a super module that exports functions, constructors etc.

#### Environment 
<p> Environment folder contains the following modules: </p>

- Structs module - Environment data type consists of Config, Logger, it also contains default number of repetitions and help message (the message that a user gets when sends /help command).
- Functions module - contains some getters wrapped in ReaderT monad, and updateConfig and eSetOffset functions.
- SetEnvironment module - contains functions to build enviroment based on information in .config file.
- Exports module - a super module that exports functions, constructors etc.

<p> The <b>Logger</b> folder contains the following modules: there are data structures in Structs module, functions for creating Logger and making LogMessage in Functions module. Exports module exports functions, constructors etc.

#### Exceptions 
<p> Exceptions folder contains the following modules: </p>

- Structs module - contains BotException data type and instances
- Functions module - contians throw exception functions and handlers
- Exports module - a super module that exports functions, constructors etc.

#### Logic
<p> Logic folder contains the following modules: </p>

- Main module - contains the processMsgs function, that handles a list of PureMessages. The module also contains some additional that are called in processMsgs functions. There are some functions for processing Callback messages in Callback module (ProcMsgs folder), and some functions for processing Common messages in Common module (ProcMsgs folder)
- PureStructs module - exports constuctors, text data etc 

<p>You can find data structures of PureMessages, Buttons etc... in <b>Structs</b> folder.</p>

#### Services 
<p> Services folder contains the following folders: </p>

- API folder - the Handle module inside contains the Handle data type, which consists of two functions for communicating to a server: hGetUpdates (for connecting to the server to get updates) and hSendMessage (for sending messages to users). The folder also containes 'new' function (for crating IO Handle)
- DB folder - the Handle module inside contains the Handle data type, which consists of three functions for communicating to a database: findUser (for looking up a user in the DB), addUSer (for inserting a new user into DB) and updateUser (for updating the number of repetitions for a user).The Handle module also has a 'new' functions (for creating IO Handle). There are two modules in <b> Database </b> folder here. It consists of Functions module (functions for connecting to a DB) and Queries module (contains the queries to a DB).
- ServiceHandle folder - contains Structs module (SHandle data type combines API Handle and DB Handle, the module also provides the 'new' function for creating IO SHandle) and Functions module (contains getters for API Handle and DB Handle functions).
<p><b>Exports</b> module here exports constructors and functions for the services described above. </p>

#### TextMessages 
<p> TextMessages folder contains all the text messages used in the project. The folder contains the following modules: </p>

- LoggerMessages - the module exports all the different text messages for logging (from LoggerMessages folder here). 
- ParseFailMessage - the module contains a message for parseFail function
- RepeatCommandMessages - the module contains some text messages user gets when sends /repeat command



## What the bot can do 

<p> The bot can get a message from user (any kinds of messages) and then send the message back to user several times. If user sends "/help" command, bot sends user some information about itself. If user sends "/repeat" command, bot sends user an inline keyboard, so user can choose the number of repetitions.</p>

## How to run

- clone the repo
- build project with stack
- check and fill in a config.cofig file (see below)
- create a database (see below)
- run an execute file

## How to fill in the Config File (config.config):

<p><b>botType</b>: Either "VK" or "Telegram" (quotes are required); </p>
<p><b>repetition</b>: an Int (recommended from 1 to 5);</p>
<p><b>telegramToken</b>: put your Telegram Token here if you want to run a Telegram Bot (quotes are required);</p>
<p><b>VKToken</b>: put your VK Token here if you want to run a VK Bot  (quotes are required);</p>
<p><b>VKGroupID</b>: an Int, put your VK Group ID here if you want to run a VK Bot;</p>
<p><b>helpMessage</b>: you can change the message a user will get if sends /help command (quotes are required);</p>
<p><b>logPriority</b>: use "Debug", "Info", "Warning" or "Error" (quotes are required).</p>
<p><b>dbConnectString</b>: a string for connecting to the database (quotes are required)</p>

## How to create a database:
<p>The bot works with a primitive database (PostreSQL), that contains one table to store user IDs and number of repetition for every user. The database supposed to be hosted locally (the same computer as the bot). So to create the database you just have to write a few strings in psql command line: </p> 

- to create a new user 'bot' with password 'bot13': <br> 
    <b> CREATE USER bot WITH password 'bot13'; </b>
- to create a new database 'users': <br> 
    <b> CREATE DATABASE users; </b>
- to grant the database to user 'bot':<br> 
    <b> GRANT ALL ON DATABASE users TO bot; </b>
- then run the database 'users' and create a new table: <br>
    <b>  CREATE TABLE UserRepeats <br>
    ( userId char (15) NOT NULL, <br>
    repeats integer NOT NULL, <br>
    CHECK ( repeats > 0 and repeats < 6), <br>
    PRIMARY KEY ( userId ));</b>
- that's all

## How it works
<p> You can see some screenshots below. </p>

### Telegram bot 
<p> Usual text message (default number of repetition is 2) and /help command response </p>
<p> <img src="https://user-images.githubusercontent.com/30144022/103772217-30ae8100-503a-11eb-9e66-017d6053ddae.png">
</p>

<p> /repeat command response and text message (number of repetition is 3) </p>
<p> <img src="https://user-images.githubusercontent.com/30144022/103865354-1fff1900-50d5-11eb-8fc8-e904bf8b7c40.png">
</p>

### VK bot 
<p> Usual text message (default number of repetition is 2) and /help command response </p>
<p> <img src="https://user-images.githubusercontent.com/30144022/103773538-46bd4100-503c-11eb-806f-f7ff7d75f1ed.png">
</p>

<p> /repeat command response and text message (number of repetition is 5) </p>
<p> <img src="https://user-images.githubusercontent.com/30144022/103865404-360cd980-50d5-11eb-98b7-14634ea570d5.png">
</p>

