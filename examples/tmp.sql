create or replace function roles_privileges_insert()
	returns trigger as
$$
begin
	-- FIXME: test taskmarket
	-- TODO: test taskmarket
	/**
	  FIXME: test taskmarket
	  TODO: test taskmarket
	*/
	/*!
	  FIXME: test taskmarket
	  TODO: test taskmarket
	*/
	if exists(select 1 from privileges
			   where id = new.privilege_id and deleted_at is null) then
		return new;
	else
		raise exception 'privilege % not found', new.privilege_id;
	end if;
end;
$$ language plpgsql;

CREATE TYPE provider_type AS ENUM ('internal', 'keycloak');

CREATE TABLE user_provider (
  id BIGSERIAL PRIMARY KEY,
  provider_type provider_type NOT NULL,
  user_id BIGINT REFERENCES users(id) NOT NULL,
  external_user_id VARCHAR(250) NOT NULL,
  UNIQUE(provider_type, user_id),
  UNIQUE(provider_type, external_user_id)
);
