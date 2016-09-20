"use strict";

if (!Detector.webgl) Detector.addGetWebGLMessage();

var camera, controls, scene, renderer;

// Globals for simplicity
var
    gRocket = {
        obj: undefined,         // Scene object
        acceleration: 0.25,     // Acceleration
        directionActual: new THREE.Vector3(),   //
        position: new THREE.Vector3(),          //
        directionDesired: new THREE.Vector3(),  //
        speed: 0.0,             // Actual movement speed
        maxSpeed: 4.0,          // Maximum movement speed
        turning_rad: 1.0,
        prevTime: 0,
        color: 0xffffff         // Color
    },
    gTarget = {
        obj: undefined,         // Scene object
        position: new THREE.Vector3(),
        color: 0x0080ff         // Color
    },
    gTrail = {
        obj: undefined,         // Scene object
        maxPoints: 300,
        prevTime: 0,
        color: 0xffffff         // Color
    };

init();
animate();

function init() {
    scene = new THREE.Scene();

    renderer = new THREE.WebGLRenderer({antialias: true});
    renderer.setPixelRatio(window.devicePixelRatio);
    renderer.setSize(window.innerWidth, window.innerHeight);
    document.body.appendChild(renderer.domElement);

    // camera
    camera = new THREE.PerspectiveCamera(40, window.innerWidth / window.innerHeight, 1, 1000);
    camera.position.set(15, 20, 30);
    scene.add(camera);

    // controls
    controls = new THREE.OrbitControls(camera, renderer.domElement);
    controls.minDistance = 20;
    controls.maxDistance = 50;
    controls.maxPolarAngle = Math.PI / 2;

    scene.add(new THREE.AmbientLight(0x222222));

    var light = new THREE.PointLight(0xffffff, 1);
    camera.add(light);

    scene.add(new THREE.AxisHelper(5));

    initSceneObjects();

    //todo: Rig UI controls

    window.addEventListener('resize', onWindowResize, false);
}

function initSceneObjects() {
    // Construct "Rocket"
    {
        gRocket.directionActual.set(0, 1, 0);
        gRocket.position.set(0, 0, 0);
        gRocket.obj = new THREE.Group();
        scene.add(gRocket.obj);

        var mtlRocket = new THREE.MeshLambertMaterial({color: gRocket.color});
        var geoRocket = new THREE.CylinderGeometry(1, 1, 8, 24);
        var meshRocket = new THREE.Mesh(geoRocket, mtlRocket);
        gRocket.obj.add(meshRocket);
    }

    // Construct "Target"
    {
        gTarget.obj = new THREE.Group();
        scene.add(gTarget.obj);

        var mtlTarget = new THREE.MeshLambertMaterial({color: gTarget.color});
        var geoTarget = new THREE.CubeGeometry(1, 1, 1);
        var meshTarget = new THREE.Mesh(geoTarget, mtlTarget);
        gTarget.obj.add(meshTarget);
    }

    // Create "Trail"
    {
        var geoTrail = new THREE.BufferGeometry();
        var positions = new Float32Array(gTrail.maxPoints * 3); // 3 vertices per point
        geoTrail.addAttribute('position', new THREE.BufferAttribute(positions, 3));
        // draw range
        geoTrail.setDrawRange(0, gTrail.maxPoints);
        var material = new THREE.LineBasicMaterial({color: gTrail.color, linewidth: 2});
        gTrail.obj = new THREE.Line(geoTrail, material);
        scene.add(gTrail.obj);
    }
}

function onWindowResize() {
    camera.aspect = window.innerWidth / window.innerHeight;
    camera.updateProjectionMatrix();

    renderer.setSize(window.innerWidth, window.innerHeight);
}

function animate() {
    requestAnimationFrame(animate);

    updateState();
    render();
}

function render() {
    renderer.render(scene, camera);
}

function updateState() {
    var time = performance.now() / 1000;

    //todo: Fix big delta on window focus change

    // Update Rocket
    {
        var delta = time - gRocket.prevTime;

        // Desired direction towards target
        var diff = gTarget.position.clone().sub(gRocket.position).normalize();
        gRocket.directionDesired.set(diff.x, diff.y, diff.z);

        // Random variation (to avoid deadlocks)
        var r = new THREE.Vector3(Math.random() - 0.5, Math.random() - 0.5, Math.random() - 0.5).normalize().multiplyScalar(0.1);

        //todo: Calculate direction properly
        // Correction vector
        var correctionVec = gRocket.directionDesired.clone()/*.add(gRocket.directionActual)*/.add(r).normalize();
        var correction = correctionVec.multiplyScalar(gRocket.turning_rad * delta);

        gRocket.directionActual.add(correction).normalize();

        // Propel rocket
        gRocket.speed = Math.min(gRocket.speed + gRocket.acceleration * delta, gRocket.maxSpeed);
        gRocket.position.add(gRocket.directionActual.clone().multiplyScalar(gRocket.speed * delta));
        gRocket.obj.position.set(gRocket.position.x, gRocket.position.y, gRocket.position.z);

        //todo: Align to route
        //gRocket.obj.rotation.x =
        gRocket.obj.rotation.y += delta;
        //gRocket.obj.rotation.z =

        gRocket.prevTime = time;
    }

    // Update trail every 0.1 sec
    if (time > gTrail.prevTime + 0.1) {
        var positions = gTrail.obj.geometry.attributes.position.array;
        for (var i = gTrail.maxPoints - 1, l = 0; i >= l; i--) {
            positions[(i + 1) * 3] = positions[i * 3];
            positions[(i + 1) * 3 + 1] = positions[i * 3 + 1];
            positions[(i + 1) * 3 + 2] = positions[i * 3 + 2];
        }
        positions[0] = gRocket.position.x;
        positions[1] = gRocket.position.y;
        positions[2] = gRocket.position.z;

        gTrail.obj.geometry.attributes.position.needsUpdate = true;

        gTrail.prevTime = time;
    }

    //todo: Register target hit

}