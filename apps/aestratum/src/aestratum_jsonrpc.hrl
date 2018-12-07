
-define(ID_MIN, 0).
-define(ID_MAX, 16#ffffffff).

-define(HOST_MAX_SIZE, 16#ff).

-define(PORT_MIN, 1).
-define(PORT_MAX, 16#ffff).

-define(USER_MAX_SIZE, 64).

-define(USER_AGENT_MAX_SIZE, 64).

-define(SESSION_ID_SIZE, 16).

-define(PASSWORD_SIZE, 64).

-define(TARGET_SIZE, 64).

-define(JOB_ID_SIZE, 16).

-define(BLOCK_VERSION_MIN, 1).
-define(BLOCK_VERSION_MAX, 16#ffffffff).

-define(HEADER_HASH_SIZE, 64).

%% In seconds.
-define(WAIT_TIME_MIN, 1).
-define(WAIT_TIME_MAX, 24 * 60 * 60).

-define(POW_SIZE, 42).
-define(POW_NUMBER_MIN, 0).
-define(POW_NUMBER_MAX, 16#ffffffff).

-define(ERROR_MSG_MAX_SIZE, 16#ff).

