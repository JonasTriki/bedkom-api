-- Types
drop type user_role, study_program, semester;

create type user_role as enum ('student', 'company', 'bedkom', 'bedkom_admin', 'superadmin');
create type study_program as enum (
  'BAMN-DTEK',
  'BAMN-DSIK',
  'BAMN-DVIT',
  'BAMN-BINF',
  'BATF-IMOE',
  'BASV-IKT',
  'MAMN-INF',
  'MAMN-PROG'
  );
create type semester as enum ('spring', 'autumn');

drop table if exists users, dots, companies, menus, food_entries, presentations, organizers, registrations, waitlist;

-- Users table
create table users
(
  id varchar primary key,
  first_name varchar not null,
  last_name varchar not null,
  email varchar not null,
  study_program study_program not null,
  year char(4) not null,
  role user_role not null
);

-- Dots table
create table dots
(
  id varchar primary key,
  user_id varchar not null
    constraint user_id
      references users(id)
      on delete cascade,
  year char(4) not null,
  semester semester not null,
  dots int not null
);

-- Companies table
create table companies
(
  id varchar
    constraint companies_pk
      primary key,
  name varchar not null,
  description varchar not null,
  website varchar not null,
  banner_url varchar
);

-- Menus table
create table menus
(
  id varchar
    constraint menus_pk
      primary key,
  name varchar not null
);

-- Food entries table
create table food_entries
(
  id varchar
    constraint food_entries_pk
      primary key,
  name varchar not null,
  details varchar not null,
  menu_id varchar not null
    constraint food_entries_menus_id_fk
      references menus(id)
      on delete cascade
);

-- Presentations table
create table presentations
(
  id varchar
    constraint presentations_pk
      primary key,
  semester semester not null,
  year char(4) not null,
  capacity int not null,
  start_time timestamp not null,
  end_time timestamp not null,
  company_id varchar not null
    constraint presentations_companies_id_fk
      references companies(id)
      on delete cascade,
  menu_id varchar
    constraint presentations_menus_id_fk
      references menus(id)
      on delete cascade
);

-- Organizers table
create table organizers
(
  presentation_id varchar not null
    constraint organizers_presentations_id_fk
      references presentations(id)
      on delete cascade,
  user_id varchar not null
    constraint organizers_users_id_fk
      references users(id)
      on delete cascade
);

-- Registrations table
create table registrations
(
  presentation_id varchar not null
    constraint registrations_presentations_id_fk
      references presentations(id)
      on delete cascade,
  user_id varchar not null
    constraint registrations_users_id_fk
      references users(id)
      on delete cascade
);

-- Waitlist table
create table waitlist
(
  presentation_id varchar not null
    constraint waitlist_presentations_id_fk
      references presentations(id)
      on delete cascade,
  user_id varchar not null
    constraint waitlist_users_id_fk
      references users(id)
      on delete cascade
);