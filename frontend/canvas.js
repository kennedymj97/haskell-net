function main() {
  const container = document.getElementById('canvas-container');
  const canvas = document.getElementById('canvas');
  initCanvas(canvas, container);
}

function initCanvas(canvas, container) {
  window.addEventListener('resize', () => resizeCanvas(), false);

  function resizeCanvas() {
    canvas.width = container.offsetWidth;
    canvas.height = container.offsetHeight;
    initDraw(canvas, container, app);
  }
  resizeCanvas();
}

function initDraw(canvas, container) {
  const ctx = canvas.getContext('2d');
  ctx.fillStyle = 'white';
  ctx.fillRect(0, 0, canvas.width, canvas.height);
  canvas.addEventListener('mousedown', setPos, false);
  canvas.addEventListener('mousemove', draw, false);
  canvas.addEventListener('mouseup', sendData, false);
  canvas.addEventListener('mouseleave', sendData, false);

  function minDimension() {
    if (canvas.width < canvas.height) {
      return canvas.width;
    }

    return canvas.height;
  }

  function sendData() {
    app.ports.numChanged.send({
      data: Array.from(
        ctx.getImageData(0, 0, minDimenstion(), minDimension()).data,
      ),
    });
  }

  const resetBtn = document.getElementById('reset');
  resetBtn.onclick = function() {
    ctx.clearRect(0, 0, canvas.width, canvas.height);
    ctx.fillStyle = 'white';
    ctx.fillRect(0, 0, canvas.width, canvas.height);
  };

  let pos = {x: 0, y: 0};

  function setPos(e) {
    pos.x = e.clientX - container.offsetLeft;
    pos.y = e.clientY - container.offsetTop;
  }

  function draw(e) {
    if (e.buttons !== 1) return;

    ctx.beginPath();
    ctx.lineWidth = container.offsetWidth / 13;
    ctx.lineCap = 'round';

    ctx.moveTo(pos.x, pos.y);
    setPos(e);
    ctx.lineTo(pos.x, pos.y);

    ctx.stroke();
  }
}

main();
