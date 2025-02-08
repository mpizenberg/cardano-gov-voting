// Initialize the database and return the connection.
// The stores argument is an array of store names.
// The function returns the initialized database connection.
export async function init({ dbName = "Store", stores = [] }) {
  return new Promise((resolve, reject) => {
    const request = indexedDB.open(dbName, 2);
    request.onerror = () => reject(request.error);
    request.onsuccess = () => {
      const db = request.result;
      const dbWrite = ({ storeName, key, data }) =>
        write({ db, storeName }, { key, data });
      const dbRead = ({ storeName, key }) => read({ db, storeName }, key);
      resolve({ write: dbWrite, read: dbRead });
    };
    request.onupgradeneeded = (event) => {
      const db = event.target.result;
      for (const storeName of stores) {
        if (!db.objectStoreNames.contains(storeName)) {
          db.createObjectStore(storeName, { keyPath: "key" });
        }
      }
    };
  });
}

async function write({ db, storeName }, { key, data }) {
  const transaction = db.transaction([storeName], "readwrite");
  const store = transaction.objectStore(storeName);
  return new Promise((resolve, reject) => {
    const request = store.put({ key, data });
    request.onerror = () => reject(request.error);
    request.onsuccess = () => resolve(request.result);
  });
}

async function read({ db, storeName }, key) {
  const transaction = db.transaction([storeName], "readonly");
  const store = transaction.objectStore(storeName);
  return new Promise((resolve, reject) => {
    const request = store.get(key);
    request.onerror = () => reject(request.error);
    request.onsuccess = () => resolve(request.result?.data);
  });
}
