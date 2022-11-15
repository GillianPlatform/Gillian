type EventName = 'resetView';

export function subscribe(eventName: EventName, listener: () => void) {
  document.addEventListener(eventName, listener);
}

export function unsubscribe(eventName: EventName, listener?: () => void) {
  document.removeEventListener(eventName, listener as () => void);
}

export function publish(eventName: EventName) {
  const event = new Event(eventName);
  document.dispatchEvent(event);
}
