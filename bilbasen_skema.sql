USE bilbasen_ola;

CREATE TABLE Forhandler (
  forhandlerid VARCHAR(100) PRIMARY KEY
);

CREATE TABLE Bil (
  carid VARCHAR(30) PRIMARY KEY,
  maerke VARCHAR(100),
  model VARCHAR(255),
  registrering VARCHAR(30),
  link TEXT,
  forhandlerid VARCHAR(100),
  FOREIGN KEY (forhandlerid)
    REFERENCES Forhandler(forhandlerid)
);

CREATE TABLE BilObservation (
  observationid INT AUTO_INCREMENT PRIMARY KEY,
  carid VARCHAR(30) NOT NULL,
  scrapedate DATETIME NOT NULL,
  pris DECIMAL(12,2),
  kilometer INT,
  sold BOOLEAN DEFAULT FALSE,
  forrige_observationid INT,
  FOREIGN KEY (carid)
    REFERENCES Bil(carid),
  FOREIGN KEY (forrige_observationid)
    REFERENCES BilObservation(observationid)
);