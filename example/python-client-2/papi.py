from typing import Any, Generic, Iterator, Literal, TypeVar

from convex import ConvexClient, ConvexInt64
from pydantic import BaseModel, Field, TypeAdapter, ValidationError
from pydantic_core import core_schema


class PydanticConvexInt64(ConvexInt64):
    @classmethod
    def __get_pydantic_core_schema__(cls, s, h) -> core_schema.CoreSchema:
        from_int_schema = core_schema.no_info_after_validator_function(cls, core_schema.int_schema())

        def validate_from_instance(v):
            return PydanticConvexInt64(v.value)

        from_instance_schema = core_schema.no_info_after_validator_function(
            validate_from_instance, core_schema.is_instance_schema(ConvexInt64)
        )

        return core_schema.union_schema([from_instance_schema, from_int_schema])

T = TypeVar('T')
class Id(str, Generic[T]):
    @classmethod
    def __get_pydantic_core_schema__(cls, s, h) -> core_schema.CoreSchema:
        return core_schema.no_info_after_validator_function(cls, core_schema.str_schema())


class MessagesDoc(BaseModel):
    id: Id['MessagesDoc'] = Field(..., alias="_id")
    creation_time: float = Field(..., alias="_creationTime")
    author: str = Field(...)
    body: str = Field(...)


    class Config:
        populate_by_name: bool = True


# --- API Client Class ---

class API:
    """A type-safe client for your Convex API."""
    def __init__(self, client: ConvexClient):
        self._client = client
        self.list_messages = self.ListMessages(self._client)
        self.send_message = self.SendMessage(self._client)

    class ListMessages:
        def __init__(self, client: ConvexClient):
            self._client = client
        def list(self, ) -> list[MessagesDoc] | None:
            """Wraps the "listMessages:list" Query."""
            payload: dict[str, Any] = {}
            try:
                raw_result = self._client.query("listMessages:list", payload)
                if raw_result is None:
                    return None
                return TypeAdapter(list[MessagesDoc]).validate_python(raw_result)

            except ValidationError as e:
                print(f"Validation error in 'list': {e}")
                raise
            except Exception as e:
                print(f"Error in 'list': {e}")
                raise

        def subscribe_list(self, ) -> Iterator[list[MessagesDoc]]:
            """Subscribes to the "listMessages:list" query."""
            payload: dict[str, Any] = {}
            raw_subscription = self._client.subscribe("listMessages:list", payload)
            adapter = TypeAdapter(list[MessagesDoc])
            for raw_result in raw_subscription:
                try:
                    validated_result = adapter.validate_python(raw_result)
                    yield validated_result
                except ValidationError as e:
                    print(f"Validation error in subscription update: {e}")
                    continue



    class SendMessage:
        def __init__(self, client: ConvexClient):
            self._client = client
        def send(self, author: str, body: str) -> None:
            """Wraps the "sendMessage:send" Mutation."""
            payload: dict[str, Any] = {"author": author, "body": body}
            try:
                self._client.mutation("sendMessage:send", payload)
                return

            except ValidationError as e:
                print(f"Validation error in 'send': {e}")
                raise
            except Exception as e:
                print(f"Error in 'send': {e}")
                raise






# --- Singular Type Aliases for Ergonomics ---
Message = MessagesDoc

