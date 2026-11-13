CREATE DATABASE IF NOT EXISTS bilbasen_ola
CHARACTER SET utf8mb4;

USE bilbasen_ola;


USE bilbasen_ola;

CREATE TABLE forhandlere (
    forhandlerid INT AUTO_INCREMENT PRIMARY KEY,
    navn VARCHAR(255) NOT NULL
);


CREATE TABLE biler (
    carid VARCHAR(30) PRIMARY KEY,
    maerke VARCHAR(100),
    model VARCHAR(150),
    type VARCHAR(255),
    registrering VARCHAR(50),
    kilometer VARCHAR(50),
    km_pr_liter VARCHAR(50),
    detaljer TEXT,
    lokation VARCHAR(255),
    link TEXT,
    forhandlerid INT,
    sold BOOLEAN NOT NULL DEFAULT FALSE,

    FOREIGN KEY (forhandlerid)
        REFERENCES forhandlere(forhandlerid)
);


CREATE TABLE priser (
    prisid INT AUTO_INCREMENT PRIMARY KEY,
    carid VARCHAR(30) NOT NULL,
    pris DECIMAL(12, 2) NOT NULL,
    scrapedate DATE NOT NULL,
    forrige_prisid INT,

    FOREIGN KEY (carid)
        REFERENCES biler(carid),

    FOREIGN KEY (forrige_prisid)
        REFERENCES priser(prisid),

    UNIQUE (carid, scrapedate)
);
