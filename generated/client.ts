import type { GREETING_Input } from './types';

class CLRPCClient {
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

  public createCaller<TInput, TOutput>(
    procedureName: string,
    type: 'query' | 'mutation' = 'query'
  ) {
    return {
      query: async (input: TInput): Promise<TOutput> => {
        return this.makeRequest<TInput, TOutput>(procedureName, type, input);
      },
    };
  }
}

const clrpc = new CLRPCClient('http://localhost:3000');

// Create type-safe procedure callers
export const greeting = clrpc.createCaller<GREETING_Input, string>('greeting');
