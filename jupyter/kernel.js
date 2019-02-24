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
    //Jupyter.HELLO = HELLO;
 //   const Jupyter = window.Jupyter;
//    Jupyter.actions.register()
    // do more things here, like define a codemirror mode

  }}

});

