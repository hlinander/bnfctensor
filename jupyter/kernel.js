function hashCode(str) { // java String#hashCode
    var hash = 0;
    for (var i = 0; i < str.length; i++) {
       hash = str.charCodeAt(i) + ((hash << 5) - hash);
    }
    return hash;
} 

function intToRGB(i){
    var c = (i & 0x00FFFFFF)
        .toString(16)
        .toUpperCase();

    return "00000".substring(0, 6 - c.length) + c;
}

const registerIndexHover = () => {
    var tensors = document.getElementsByClassName('index');
    console.log('HELLO UPDATE');
    Array.from(tensors).forEach(function (el) {
        if (!el.hoverEventRegistered) {
            el.hoverEventRegistered = 1;
            el.addEventListener('mouseover', (event) => {
                const otherClasses =
                    Array.from(el.classList.values()).filter(cn => cn != 'index' && cn != 'hoverIndex');
                const otherTensors = document.getElementsByClassName(otherClasses[0]);
                for (let t of otherTensors) {
                    t.classList.add('hoverIndex');
                }
            });
            el.addEventListener('mouseleave', (event) => {
                const otherClasses =
                    Array.from(el.classList.values()).filter(cn => cn != 'index' && cn != 'hoverIndex');
                const otherTensors = document.getElementsByClassName(otherClasses[0]);
                for (let t of otherTensors) {
                    t.classList.remove('hoverIndex');
                }
            });
        }
    });
}

const registerTensorHover = () => {
    var tensors = document.getElementsByClassName('tensor');
    console.log('HELLO UPDATE');
    Array.from(tensors).forEach(function (el) {
        if (!el.hoverEventRegistered) {
            el.hoverEventRegistered = 1;
            el.addEventListener('mouseover', (event) => {
                const otherClasses =
                    Array.from(el.classList.values()).filter(cn => cn != 'tensor' && cn != 'hover');
                const otherTensors = document.getElementsByClassName(otherClasses[0]);
                for (let t of otherTensors) {
                    t.classList.add('hover');
                }
            });
            el.addEventListener('mouseleave', (event) => {
                const otherClasses =
                    Array.from(el.classList.values()).filter(cn => cn != 'tensor' && cn != 'hover');
                const otherTensors = document.getElementsByClassName(otherClasses[0]);
                for (let t of otherTensors) {
                    t.classList.remove('hover');
                }
            });
        }
    });
}


define(function(){
  return {onload: function(){
    console.info('Kernel specific javascript loaded');

    window.registerTensorHover = registerTensorHover;
    window.registerIndexHover = registerIndexHover;
    //Jupyter.HELLO = HELLO;
 //   const Jupyter = window.Jupyter;
//    Jupyter.actions.register()
    // do more things here, like define a codemirror mode

  }}

});

