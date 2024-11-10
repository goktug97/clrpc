export interface RPCProcedure<TInput, TOutput> {
  query(input: TInput): Promise<TOutput>;
}

export class CLRPCClient<TProcedures> {
  private baseUrl: string;
  
  constructor(baseUrl: string) {
    this.baseUrl = baseUrl;
  }

  private async makeRequest<TInput, TOutput>(
    procedureName: string,
    type: 'query' | 'mutation',
    input: TInput
  ): Promise<TOutput> {
    const response = await fetch(`${this.baseUrl}/${type}/${procedureName}`, {
      method: 'POST',
      headers: {
        'Content-Type': 'application/json',
      },
      body: JSON.stringify(input),
    });
    if (!response.ok) {
      throw new Error(`CLRPC error: ${response.statusText}`);
    }
    return response.json();
  }

  createCaller<TInput, TOutput>(
    procedureName: string,
    type: 'query' | 'mutation' = 'query'
  ): RPCProcedure<TInput, TOutput> {
    return {
      query: async (input: TInput): Promise<TOutput> => {
        return this.makeRequest<TInput, TOutput>(procedureName, type, input);
      },
    };
  }
}

export function createCLRPCClient<TProcedures>(baseUrl: string): CLRPCClient<TProcedures> & TProcedures {
  const client = new CLRPCClient<TProcedures>(baseUrl) as CLRPCClient<TProcedures> & TProcedures;

  return new Proxy(client, {
    get(target, prop) {
      if (prop in target) {
        return (target as any)[prop];
      }

      return target.createCaller(prop as string);
    }
  }) as CLRPCClient<TProcedures> & TProcedures;
}
