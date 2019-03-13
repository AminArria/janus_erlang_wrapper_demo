var localVideo;
var localStream;
var remoteVideo;
var peerConnection;
var serverConnection;
var sessionId;
var handleId;
var publisherId;
var roomInput;
var feedInput;
var isPublisher = false;

const CONFIG = {
  audio: true,
  video: true,
  iceServers: [
    {urls: 'stun:stun.l.google.com:19302'},
  ]
}

const RECEIVERS = {
  create_session,
  create_handle,
  create_room,
  join_publisher,
  publish,
  join_subscriber,
}

function parse_payload_receiver(payload) {
  if(payload.session_id) {
    return create_session;
  } else if(payload.handle_id) {
    return create_handle;
  } else if(payload.room_id) {
    return create_room;
  } else if(payload.publisher_id) {
    return join_publisher;
  } else if(payload.jsep && isPublisher) {
    return publish;
  } else if (payload.jsep) {
    return join_subscriber;
  } else {
    return console.log;
  }
}

function page_ready() {
  localVideo = document.getElementById('localVideo');
  remoteVideo = document.getElementById('remoteVideo');
  roomInput = document.getElementById('room_id');
  feedInput = document.getElementById('feed_id');

  serverConnection = new WebSocket('ws://' + window.location.hostname + ':8080/websocket');
  serverConnection.onmessage = function(message) {
    payload = JSON.parse(message.data);
    receiver = parse_payload_receiver(payload);
    receiver(payload);
  }
}

function start_session() {
  msg = JSON.stringify({request: 'create_session'});
  serverConnection.send(msg);
}

function start_handle() {
  msg = JSON.stringify({request: 'create_handle', session_id: sessionId});
  serverConnection.send(msg);
}

function make_room() {
  msg = JSON.stringify({request: 'create_room', session_id: sessionId, handle_id: handleId});
  serverConnection.send(msg);
}

function join_room_publisher() {
  roomId = parseInt(roomInput.value);
  msg = JSON.stringify({request: 'join_publisher', room_id: roomId, session_id: sessionId, handle_id: handleId});
  serverConnection.send(msg);
}

function start_broadcast() {
  peerConnection = new RTCPeerConnection(CONFIG);
  peerConnection.onicecandidate = on_ice_candidate;
  peerConnection.ontrack = on_track;

  navigator.mediaDevices.getUserMedia(CONFIG).then(function(stream) {
    localVideo.srcObject = stream;
    peerConnection.addStream(stream);

    peerConnection.createOffer().then(function(offer) {
      peerConnection.setLocalDescription(offer);
      msg = JSON.stringify({request: 'publish', jsep: offer, session_id: sessionId, handle_id: handleId});
      serverConnection.send(msg);
    });
  });
}

function join_broadcast() {
  roomId = parseInt(roomInput.value);
  feedId = parseInt(feedInput.value);

  peerConnection = new RTCPeerConnection(CONFIG);
  peerConnection.onicecandidate = on_ice_candidate;
  peerConnection.ontrack = on_track;

  msg = JSON.stringify({request: 'join_subscriber', room_id: roomId, feed_id: feedId, session_id: sessionId, handle_id: handleId});
  serverConnection.send(msg);
}

// RCTPeerConnection on
function on_ice_candidate(event) {
  msg = JSON.stringify({request: 'trickle', candidate: event.candidate, session_id: sessionId, handle_id: handleId});
  serverConnection.send(msg);
}

function on_track(event) {
  remoteVideo.srcObject = event.streams[0];
}

// Receivers
function create_session(payload) {
  sessionId = payload.session_id;
  setInterval(keepalive, 30);
}

function create_handle(payload) {
  handleId = payload.handle_id;
}

function create_room(payload) {
  roomId = payload.room_id;
  roomInput.setAttribute('value', roomId);
}

function join_publisher(payload) {
  isPublisher = true;
  feedInput.setAttribute('value', payload.publisher_id);
}

function publish(payload) {
  answer = payload.jsep;
  peerConnection.setRemoteDescription(new RTCSessionDescription(answer));
  // peerConnection.addIceCandidate(new RTCIceCandidate(answer.sdp));
}

function join_subscriber(payload) {
  peerConnection.setRemoteDescription(new RTCSessionDescription(payload.jsep));
  peerConnection.createAnswer().then(function(answer) {
    peerConnection.setLocalDescription(answer);
    msg = JSON.stringify({request: "listen", jsep: answer, session_id: sessionId, handle_id: handleId});
    serverConnection.send(msg);
  });
}

function keepalive() {
    msg = JSON.stringify({request: 'keepalive', session_id: sessionId});
    serverConnection.send(msg);
}
