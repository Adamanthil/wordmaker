<?php
require __DIR__ . '/vendor/autoload.php';

$GLOBALS['config'] = include('config.php');

// Use the REST API Client to make requests to the Twilio REST API
use Twilio\Rest\Client;

if (strtoupper($_SERVER['REQUEST_METHOD']) == 'POST') {
	if(isset($_POST['message'])) {
		sendSms($_POST['message']);
	}
	else {
		sendSms(getVisitMessage());
	}
}

function sendSms($message) {
	// Your Account SID and Auth Token from twilio.com/console
	$sid = $GLOBALS['config']['twilio']['sid'];
	$token = $GLOBALS['config']['twilio']['auth_token'];
	$client = new Client($sid, $token);

	// Use the client to do fun stuff like send text messages!
	$client->messages->create(
	    // the number you'd like to send the message to
	    $GLOBALS['config']['twilio']['to_number'],
	    array(
	        'from' => $GLOBALS['config']['twilio']['from_number'],
	        'body' => $message
	    )
	);
}

function getVisitMessage() {
	return "WordMaker has been visited\nby " . getUserAgentDisplay() . "\nin " . getIPLocationDisplay();
}

function getUserAgentDisplay() {
	$userAgentURL = 'https://useragentapi.com/api/v3/json/' . $GLOBALS['config']['useragent']['api_key'] . '/';
	$userAgent = json_decode(file_get_contents($userAgentURL . urlencode($_SERVER['HTTP_USER_AGENT'])));

	return $userAgent->data->platform_type . ' '
		. $userAgent->data->browser_name. ' '
		. $userAgent->data->browser_version . "\nfor "
		. $userAgent->data->platform_name;
}

function getIPLocationDisplay() {
	$ipApiUrl = 'http://ip-api.com/json/';
	$ipLocation = json_decode(file_get_contents($ipApiUrl . $_SERVER['REMOTE_ADDR']));

	if($ipLocation->status == "success") {
		return $ipLocation->city . ', ' . $ipLocation->region;
	}
}
