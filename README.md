# Echo bot for Telegram and VK 

<p> The Telegram bot works properly and supports any types of messages (with or without attachments).  </p>
<p> The VK bot can process text messages, /repeat and /help commands, map coordinates, links, stickers, audio and video attachments. If there is an attachment VK bot cannot process, the attachment will be ignored.   </p>

## Project structure description

<p> The project is devided into three parts: app (Main module), src (see the structure of this part below), test (contains unit tests). </p> 

### Src structure 

<p> The src part contains the Bot module and the following folders: </p>

- API  
- Config
- Environment 
- Exceptions  
- Logger
- Logic  
- Services  
- TextMessages
- Wrapper

#### Bot module
<p> The Bot module exports runBot function (called in the Main module). The function holds a chain of actions: the bot gets updates, handles them and then call the nextLoop function (it prints the log message and calls runBot again). </p>

#### API 
<p> The API folder contains three folders: PureStructs, VK and Telegram. The aim of the functions, actions and structures of these folders is to decode bytestrings into specific structures (Telegram structures or VK ones) and then build PureMessages. </p>

<p> The PureStructs folder contains some data structures the Telegram or VK messaged supposed to be decoded to. The folder contains the following modules: </p>

- Buttons - there is a PureButtons data type and a set of PureButtons for building inline keyboard, and some text information for making callback text.
- PureMessages - the module contains data structure for PureMessages and some aliases. 
- Exports - the module exports constructors, functions and data from Buttons and PureMessages modules.

<p>The Telegram folder contains the following:</p>

- Main - the module exports the decodePureMessageList function (the function is called in Wrapper.Decoders module), it decodes bytestring into TelegramUpdates and builds PureMessage. There are some other helper functions for decoding, throwing exceptions and building PureMessage list in this module.
- URL - the modue contains a set of funtions for building urls.
- The Functions folder - contains Builders module (that exports buildPureMessage function), Params module (contains some functions for building message parameters), Attachments module (exports some functions for builing PureMessages of specific message types).
- The Structs folder - contains specific Telegram data structures.

<p>The VK folder contains the following:</p>

- Main - the module contains the decodePureMessageList function (called in Wrapper.Decoders module), that decodes bytestring into VKUpdates and then builds PureMessage. And there are some other helping functions.
- URL - there is a set of funtions for building urls in this module.
- The Functions folder - contains Builders module (that exports buildPureMessage function), Params module contains some functions for building PureMEssage parameters (the module imports functions for building parameters from Params folder), MessageTypes module contains buildMessageType functions (it imports some functions from MessageTypes folder for builing the correct message type).
- The Structs folder - contains specific VK data structures

#### Config 
<p> The Config folder holds all the necessary things to initialize and use Config. There are the following modules inside: </p>

- Initialization - the module exports setBotSettings function (it builds Config based on information from .config file). The module also contains some other helping functions. 
- Function - the module holds setOffset functions for updating Config. 
- Data - the module contains some data for building urls. 
- Exports - the module exports functions, constructors and data. 
- Structs - the module contains some data structures for config and some aliases.

#### Environment
<p>Environment is a data structure that holds all the necessary information such as current bot config, logger, and some other data from .config file. The Environment folder contains the following modules: </p>

- Initialization - the module exports the setEnvironment function (called in Main module), it also holds some other helping functions for getting information from .config file and initializing Environment.
- Functions - the module contains getters for some data stored in Environment and wrapped in ReaderT monad. 
- Structs - it contains the Environment data structure and some aliases. 
- Exports - the module exports some functions and constructors.

#### Exceptions 
<p> The Exceptions folder contains BotException data structs, MonadThrow typeclass definition, a set of functions that throw different types of exceptions and an exception handle function. The folder contains the following modules:</p>

- Structs - the module contains BotExpeption data type and instances. 
- Functions - it contains MonadThrow typeclass definition and an instance for IO Monad. It also holds a set of functions for throwing and handling exception. 
- Exports - the module exports functions, constructor and instances.

#### Logger 
<p> The Logger folder holds all the necessary things to initialize and use logger. It contains the following modules: </p> 

- Initialization - the module contains createLogger function. 
- Structs - there are specific data structures for logger in Structs module. 
- Functions - you can find a createLogMessage function here. 
- Exports - the module contains functions, data structs and constructors for export. 

#### Logic
<p> The Logic folder contains all the necessary things for handling pure messages. Depending on the message type, the message may be ignored (for Empty messages), sent back to user several times (for Common messages). If it is a UserCommand message, user gets a special answer, and if it is a Callback message some information should be changed in a database. The folder contains the following:</p>

- Main - the module exports the processMsgs function, that supposed to handle pure messages. The module also contains some additional helper functions. 
- The Functions folder - holds two modules: the Callback module contains functions for processing Callback messages and the Common module contains functions for processing Common messages.

#### Services 
<p> The Services folder provides all the necessary services for connecting to a server (for getting updates and sending messages) and connecting to a database (for looking for users, adding new users and updating user information). The Services folder contains the following: </p>

- Main - the module contains Services typeclass definition and an instance for IO monad. The module exports Services constructor with functions and instances. 
- FunctionsIO - the module contains a set of functions for IO instance of Services typeclass. 
- The API folder - the Handle module contains the Handle data type, which consists of two functions for communicating with a server: hGetUpdates (for connecting to the server to get updates) and hSendMessage (for sending messages to users). The folder also containes 'new' function (for creating IO Handle), 'close' function (for closing IO Handle) and withAPIHandle function (for working with Handle). 
- DB folder - the Handle module contains the Handle data type, which consists of three functions for communicating with a database: findUser (for looking up a user in the DB), addUSer (for inserting a new user into DB) and updateUser (for updating the number of repetitions for a user).The Handle module also has 'new', 'close' and 'withDBHandle' functions. There are two modules in Database folder here: Functions module (functions for connecting to a DB) and Queries module (contains the SQL queries for DB).

#### TextMessages 
<p> The TextMessages folder contains all the text messages used in the project. The folder contains the following modules: </p>

- LoggerMessages - the module exports all the different text messages (wrapped in LogMessage) for logging (from LoggerMessages folder here). 
- ParseFailMessage - the module contains a message for parseFail function
- RepeatCommandMessages - the module contains some text messages user gets when sends /repeat command

#### Wrapper 
<p> The Wrapper folder contains all the necessary instruments for building requests to server and decoding server responses. The folder contains the following:</p>

- Main - the module exports getUpdates and sendMessage functions. It also contains one more helper function. 
- The URL folder - it holds three modules: the Structs module contains a data structure for building url; the Telegram module contains some functions for building Telegram urls; and there are some functions for building VK urls in VK module.
- The Functions folder - contains Actions module (it exports updateEnvironment functions for setting new offset). Decoders module contains functions for decoding server response to bytestring and decoding bytestring to PureMessage. Requests module exports sendMessageRequest and getUpdatesRequest functions (for connecting to a server and getting server response), the module also holds some additional helper functions. URL module contains some functions for building urls.

## What the bot can do 

<p> The bot can get a message from user (any kinds of messages) and then send the message back to the user several times. If the user sends "/help" command, the bot sends some information about itself. If the user sends "/repeat" command, the bot sends an inline keyboard, so the user can choose the number of repetitions.</p>

## How to run

- clone the repo
- build the project with stack
- check and fill in a config.cofig file (see below)
- create a database (see below)
- run an execute file

## How to fill in the Config File (config.config):

<p><b>botType</b>: Either "VK" or "Telegram" (quotes are required); </p>
<p><b>repetition</b>: an Int (recommended from 1 to 5);</p>
<p><b>telegramToken</b>: put your Telegram Token here if you want to run a Telegram Bot (quotes are required);</p>
<p><b>VKToken</b>: put your VK Token here if you want to run a VK Bot  (quotes are required);</p>
<p><b>VKGroupID</b>: an Int, put your VK Group ID here if you want to run a VK Bot;</p>
<p><b>helpMessage</b>: you can change the message a user gets if sends /help command (quotes are required);</p>
<p><b>logPriority</b>: use "Debug", "Info", "Warning" or "Error" (quotes are required).</p>
<p><b>dbConnectString</b>: a string for connecting to a database (quotes are required)</p>

## How to create a database:
<p>The bot works with a primitive database (PostreSQL), that contains one table to store user IDs and number of repetition for every user. The database supposed to be hosted locally. So to create the database you just have to write a few strings in psql command line: </p> 

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

