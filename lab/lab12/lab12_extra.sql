.read lab12.sql

-- Q5
CREATE TABLE greatstudents AS
  SELECT a.date, a.color, a.pet, a.number, b.number FROM students as a, fa17students as b WHERE a.date = b.date AND a.color = b.color AND a.pet = b.pet;

-- Q6
CREATE TABLE sevens AS
  SELECT a.seven FROM students as a, checkboxes as b WHERE a.time = b.time AND a.number = 7 AND b.'7' = 'True';

-- Q7
CREATE TABLE fa17favnum AS
  SELECT number, COUNT(*) as count FROM fa17students GROUP BY number ORDER BY count DESC LIMIT 1;


CREATE TABLE fa17favpets AS
  SELECT pet, COUNT(*) as count FROM fa17students GROUP BY pet ORDER BY count DESC LIMIT 10;

CREATE TABLE sp18favpets AS
  SELECT pet, COUNT(*) as count FROM students GROUP BY pet ORDER BY count DESC LIMIT 10;


CREATE TABLE sp18dog AS
  SELECT "dog", COUNT(*) FROM students WHERE pet = 'dog';


CREATE TABLE sp18alldogs AS
  SELECT "dog", COUNT(*) FROM students WHERE pet LIKE '%dog%';


CREATE TABLE obedienceimages AS
  SELECT seven, denero, COUNT(*) FROM students WHERE seven = '7' GROUP BY denero;

-- Q8
CREATE TABLE smallest_int_count AS
  SELECT smallest, COUNT(*) FROM students GROUP BY smallest;
