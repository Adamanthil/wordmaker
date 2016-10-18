<?php
require __DIR__ . '/vendor/autoload.php';

$config = include('config.php');

// Use the REST API Client to make requests to the Twilio REST API
use Twilio\Rest\Client;

// Your Account SID and Auth Token from twilio.com/console
$sid = $config['sid'];
$token = $config['auth_token'];
$client = new Client($sid, $token);

// Use the client to do fun stuff like send text messages!
$client->messages->create(
    // the number you'd like to send the message to
    $config['to_number'],
    array(
        'from' => $config['from_number'],
        'body' => 'Word Puzzle has been visited!'
    )
);
