program MapViewer2
	use, intrinsic :: iso_c_binding
	! the credentials file is ignored by git. it contains 4 constants, but only 2 are used.
	! they are USERNAME_C and PASSWORD_C. see the README for more information.
	use :: mqttCredentials
	use :: paho
	use :: cFuncs
	use :: grid
	use :: gfx, only: gfxInit, gfxDraw, gfxQuit, exitClicked
	implicit none

	character(len=*), parameter :: ADDRESS = "robomqtt.cs.wpi.edu"
	character(len=*), parameter :: CLIENT_ID = "MapViewer2"

	character(len=*), parameter :: ROOT_TOPIC = "team17"
	character(len=*), parameter :: TOPIC_PREFIX = ROOT_TOPIC // "/"

	integer, parameter :: QOS = 1

	type(c_ptr) :: mqttClient
	type(mqtt_client_connect_options) :: connectionOptions = MQTT_CLIENT_CONNECT_OPTIONS_INITIALIZER

	integer :: retCode

	write(*,"(A)") "Attempting to initialize SDL2."
	if (.not. gfxInit()) stop
	write(*,"(A)") "SDL2 successfully initialized."

	! set up connection options
	connectionOptions%user_name = c_loc(USERNAME_C)
	connectionOptions%password = c_loc(PASSWORD_C)
	connectionOptions%keep_alive_interval = 20
	connectionOptions%clean_session = 1

	! attempt to connect
	write(*,"(A)") "Creating MQTT Client."
	retCode = mqtt_client_create(mqttClient, ADDRESS // c_null_char, CLIENT_ID // c_null_char, MQTTCLIENT_PERSISTENCE_NONE, c_null_ptr)
	write(*,"(A,i0,A)") "Client creation returned ", retCode, "."

	retCode = mqtt_client_set_callbacks(mqttClient, &
		c_null_ptr, &
		c_funloc(onConnectionLost), &
		c_funloc(onMessageArrived), &
		c_null_ptr) !c_funloc(onDeliveryComplete))

	write(*,"(3A)") "Attempting to connect to ", ADDRESS, "."
	retCode = mqtt_client_connect(mqttClient, connectionOptions)
	if (retCode == MQTTCLIENT_SUCCESS) then
		write(*,"(A)") "Successfully connected."

		retCode = mqtt_client_subscribe(mqttClient, TOPIC_PREFIX // "#" // c_null_char, QOS)
		if (retCode == MQTTCLIENT_SUCCESS) then
			write(*,"(3A)") "Successfully subscribed to ", TOPIC_PREFIX // "#", "."
		end if

!		block
!			character(len=*), parameter :: TOPIC = TOPIC_PREFIX // "gridSize"
!			character(len=*), parameter :: MESSAGE = "10"
!			call publishMessage(mqttClient, TOPIC, MESSAGE)
!		end block

		!call publishMessage(mqttClient, TOPIC_PREFIX // "gridData/0 1", "0")

		! wait for exit
		do while (.true.)
			call gfxDraw()
			if (exitClicked) then
				exit
			end if
		end do

		retCode = mqtt_client_disconnect(mqttClient, 10000)
		call mqtt_client_destroy(mqttClient)
	else
		write(*,"(A,i0)") "Failed to connect. Error Code: ", retCode
	end if

	write(*,"(A)") "Exiting."
	call gfxQuit()

	contains
		subroutine publishMessage(client, topic, message)
			type(c_ptr) :: client
			character(len=*), intent(in) :: topic
			character(len=*), intent(in) :: message

			! not the most ideal way of handling this.
			! if something causes this to go out of scope (such as the onDeliveryComplete
			! callback) while mqtt is still using it, a segfault will occur.
			character(len=:, kind=c_char), target, allocatable, save :: payloadBuffer
			integer(c_long), parameter :: timeout = 1000
			type(mqtt_client_message) :: msg
			integer :: rc, token
			
			payloadBuffer = message // c_null_char
			msg = MQTT_CLIENT_MESSAGE_INITIALIZER
			msg%payload = c_loc(payloadBuffer)
			msg%payload_len = len(message) + 1 ! length includes null terminator
			msg%qos = 1
			msg%retained = 0

			write(*, "(5A,i0,A)") "Writing message '", message, "' to topic '", topic, "'. Timeout is ", timeout, "ms."
			rc = mqtt_client_publish_message(client, topic // c_null_char, msg, token)
			write(*, "('Publish message returned ',i0,'. Awaiting completion with token ',i0)") rc, token
			rc = mqtt_client_wait_for_completion(client, token, timeout)
			write(*, "('Wait for completion returned ',i0)") rc
			deallocate(payloadBuffer)
		end subroutine publishMessage

		subroutine onDeliveryComplete(context, deliveryToken) bind(c)
			type(c_ptr), intent(in), value :: context
			integer(c_int), intent(in) :: deliveryToken

			write(*,"('Delivery confirmed for message with token ',i0)") deliveryToken
		end subroutine onDeliveryComplete

		integer(c_int) function onMessageArrived(context, topic, topicLen, message) bind(c)
			type(c_ptr), intent(in), value :: context
			type(c_ptr), intent(in), value :: topic
			integer(c_int), intent(in), value :: topicLen
			type(c_ptr), intent(in), value :: message

			integer(c_int), target :: cLen
			character(len=:), allocatable :: topicStr
			character(len=:), allocatable :: messageStr

			! possible bug in fortran-paho: topicLen is zero. use C's strlen as a workaround.
			write(*,"(A,z0)") "cLen is at 0x", c_loc(cLen)
			call cf_strlen(topic, cLen)
			write(*,"(A,z0,A,i0,A)") "strlen reports topic string (0x", topic, ") is ", cLen, " characters long."
			topicStr = mqtt_client_topic_name(topic, int(cLen, kind=c_int))
			messageStr = mqtt_client_payload(message)
			! mqtt_client_payload returns a null terminated string, so remove the terminator.
			! this seems to be caused by publishMessage() putting on a null terminator for some reason.
			if (messageStr(len(messageStr):) == c_null_char) then
				print *, "Trimming messageStr"
				messageStr = messageStr(:len(messageStr) - 1)
			end if

			write(*,"(A)") "Recieved message."
			write(*,"(4x,2A)") "Topic:", topicStr
			write(*,"(4x,2A)") "Payload:", messageStr

			! dispatch: send the message to the relevant subroutine based on topic.
			if (topicStr == TOPIC_PREFIX // "gridSize") then
				call resizeGrid(str2int(messageStr))
			else if (scan(topicStr, "gridData") > 0) then
				block
					integer :: loc
					integer, dimension(2) :: coord
					loc = scan(topicStr, "gridData/", .true.) + 1
					write(*,"(3A)") "Attempting to decode ", topicStr(loc:), " to a coordinate."
					read(topicStr(loc:),*) coord
					call updateCell(coord(1), coord(2), str2int(messageStr))
				end block
			end if

			call mqtt_client_free_message(message)
			call mqtt_client_free(topic)
			onMessageArrived = 1
			deallocate(topicStr)
		end function onMessageArrived

		subroutine onConnectionLost(context, cause) bind(c)
			type(c_ptr), intent(in), value :: context
			type(c_ptr), intent(in), value :: cause

			character(kind=c_char), pointer, dimension(:) :: cause_ptrs
			character(len=100) :: cause_str

			call c_f_pointer(cause, cause_ptrs, shape=[len(cause_str)])
			call c_f_str_chars(cause_ptrs, cause_str)

			write(*, "(A)") "Connection lost."
			write(*, "(4x,2A)") "Cause: ", cause_str
		end subroutine onConnectionLost

		integer function str2int(str)
			character(len=*), intent(in) :: str
			integer :: ret
			write(*,"(3A)") "Attempting to convert ", str, " to an integer."
			read(str,*) ret
			str2int = ret
		end function str2int
end program MapViewer2
