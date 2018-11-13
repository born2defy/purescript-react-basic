"use strict";

var React = require("react");
var Fragment = React.Fragment || "div";

exports.useContext_ = function(ctxObj){ 
    return function(){
      return React.useContext(ctxObj);
    };
};

exports.mkContext_ = function(ctxVal){
    return React.createContext(ctxVal);
};

exports.runContext_ = runContextImp;

function runContextImp(component, ctxObj, ctx, props) {
  var provider = ctxObj.Provider;
  var payload = elementImp(component, props);
  return React.createElement(provider, { value: ctx }, payload);
}

exports.runContextKeyed_ = runContextImp;

function elementImp(component, props) {
  return React.createElement.apply(
    null,
    [component, props].concat((props && props.children) || null)
  );
}

exports.element_ = elementImp;
exports.elementKeyed_ = elementImp;

exports.useState_ = function(initialState) {
  var state = React.useState(initialState);
  return { value: state[0], setValue: state[1] };
};

exports.useEffect_ = React.useEffect;

exports.useReducer_ = function(reducer, initialState, initialAction) {
  var state = React.useReducer(reducer, initialState, initialAction);
  return { state: state[0], dispatch: state[1] };
};

exports.readRef_ = function(ref) {
  return ref.current;
};

exports.writeRef_ = function(ref, a) {
  ref.current = a;
};

exports.useRef_ = React.useRef;

exports.empty = null;

exports.keyed_ = function(key, child) {
  return React.createElement(Fragment, { key: key }, child);
};

exports.fragment = function(children) {
  return React.createElement.apply(null, [Fragment, {}].concat(children));
};

exports.displayName = function(component) {
  return typeof component === "string"
    ? component
    : component.displayName || "[unknown]";
};

exports.unsafeSetDisplayName = function(displayName, component) {
  component.displayName = displayName;
  component.toString = function() {
    return displayName;
  };
  return component;
};
